module ParserUtil

open Parsec
open Types

let tabStopDistance = 8 // must be a power of 2


let comparePosition (a : Position) (b : Position) =
    match a.Line - b.Line with
    | 0 -> a.Col - b.Col
    | x -> x
    |> System.Math.Sign

let collectMessages (msgs : (Position * ErrorMessage) list) =
    msgs |> List.collect (fun (p,e) -> e |> List.map (fun e -> p,e))

let filterMessages (msgs : (Position * ErrorMessage) list) =
    let messages = collectMessages msgs |> Array.ofList |> Array.sortWith (fun a b -> -comparePosition (fst a) (fst b))
    match messages.Length with
    | 0 -> [   ]
    | _ -> 
        let (pos, errorType) = messages[0]
        [
            pos, [ errorType ]
        ]

type StringSegment with

    member this.GetOffsetToSkipNewline(offset: int) : int =
        match this.GetSafe(offset) with
        | '\r' -> if this.GetSafe(offset + 1) = '\n' then 2 else 1
        | '\n' -> 1
        | _ -> 0

    member this.SkipSpaceTab() : StringSegment =
        let mutable i = 0
        let mutable found = false

        while not found do
            match this.GetSafe(i) with

            | '/' when this.GetSafe(i+1) = '/' ->
                i <- i + 2
                let mutable inComment = true
                while inComment do
                    let c = this.GetSafe(i)
                    i <- i + 1
                    match c with 
                    | EOS -> inComment <- false
                    | '\n' ->
                        inComment <- false
                    | '\r' ->
                        if this.GetSafe(i) = '\n' then i <- i + 1
                        inComment <- false
                    | _ -> ()

            | ' ' | '\t' -> i <- i + 1


            | _ -> found <- true

        this[i..]

    /// Expects to be positioned at EOL
    member this.SkipEol() : StringSegment =
        let nlSize = this.GetOffsetToSkipNewline(0)
        this[nlSize..]

    member this.SkipToEol() : StringSegment =
        let mutable i = 0
        let mutable found = false

        while not found do
            match this.GetSafe(i) with
            | EOS | '\n' | '\r' -> found <- true
            | _ -> i <- i + 1

        this[i..]

    member this.SkipNewlineThenWhitespace( powerOf2TabStopDistance : int, allowFf : bool) = 
        let tabStopDistanceMinus1 = (powerOf2TabStopDistance - 1)
        if powerOf2TabStopDistance <= 0 || (powerOf2TabStopDistance &&& tabStopDistanceMinus1 <> 0) then
            failwith "powerOf2TabStopDistance must be a positive power of 2"

        let mutable found = false
        let mutable i = 0
        let mutable ind = -1
            
        let nlSize = this.GetOffsetToSkipNewline(i)

        if nlSize > 0 then
            ind <- 0
            i <- i + nlSize
            while not found do
                let c = this.GetSafe(i)

                match c with 

                | '/' when this.GetSafe(i+1) = '/' ->
                    i <- i + 2
                    let mutable inComment = true
                    while inComment do
                        let c = this.GetSafe(i)
                        match c with 
                        | EOS -> inComment <- false
                        | '\n' ->
                            i <- i + 1
                            ind <- 0
                            inComment <- false
                        | '\r' ->
                            i <- i + 1
                            if this.GetSafe(i) = '\n' then i <- i + 1
                            ind <- 0
                            inComment <- false
                        | _ ->
                            i <- i + 1

                | ' ' -> 
                    i <- i + 1
                    ind <- ind + 1
                | '\t' -> 
                    let d = tabStopDistanceMinus1 + 1 - (ind &&& tabStopDistanceMinus1);
                    i <- i + 1
                    ind <- (ind + d)
                | '\f' when allowFf -> 
                    i <- i + 1
                    ind <- 0
                | '\r'->
                    i <- i + 1
                    if this.GetSafe(i) = '\n' then i <- i + 1
                    ind <- 0
                | '\n'->
                    i <- i + 1
                    ind <- 0
                | _ ->
                    found <- true

        (this[i..], ind)

module Debug =
    let mutable log : (string -> unit)= ignore

    let mutable debugEnabled = false

    let console = Fable.Core.JS.console
    let mutable debugIndent = 0

    let escapeStr (s : string) =
        s.Replace("\n", "\\n")

    let private _spaces() =
        new System.String('.', debugIndent*2)

    let printErrorType e =
        match e with
        | Expected s -> "Expected " + s
        | Unexpected s -> "Unexpected " + s
        | Message s -> s

    let printPositionErrorTypes (p:Position, es: ErrorType list) =
        es |> List.map (fun e -> sprintf "%d:%d: %s" p.Line p.Col (printErrorType e))

    let printMsgs (msgs : (Position * ErrorMessage) list) =
        msgs |> List.collect printPositionErrorTypes

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        let resultStr (result) : string =
            match result with
            | Ok _ -> "success"
            | Error (msgs,_) -> "failure: " + (printMsgs msgs |> String.concat ",")

        (fun (state,stream) ->

            if debugEnabled then 
                sprintf "%s%A: Entering '%s': '%s' state=%s" (_spaces()) stream.pos label (stream.Value |> escapeStr) (state.ToString()) |> log
                debugIndent <- debugIndent + 1

            let result = (p ) (state,stream)

            if debugEnabled then 
                debugIndent <- debugIndent - 1
                sprintf "%s%A: Leaving '%s' (%A) state:%s" (_spaces()) stream.pos label (result |> resultStr |> escapeStr) (state.ToString()) |> log

            result)

open Debug

type UserState = 
    {
        Indentation: int
        IsTopLevel : bool
        JsxTags : string list
        Errors : (Position * ErrorMessage) list
    }
    with
        static member Create() = { Indentation = -1; IsTopLevel = false; JsxTags = []; Errors = [] }
        override this.ToString() = sprintf "[Indentation=%d]" this.Indentation

let stringToInt (s : string) : int = System.Int32.Parse s

let rtuple b a = a,b

let makeToken s s' =
    { 
        StartLine = s.startLine
        EndLine = s'.startLine
        StartCol = s.startColumn
        EndCol = s'.startColumn
    }

let mergeSourceTokens a b =
    {
        StartLine = System.Math.Min(a.StartLine, b.StartLine)
        StartCol = 
            if (a.StartLine < b.StartLine) then a.StartCol
            else if (a.StartLine > b.StartLine) then b.StartCol
            else System.Math.Min( a.StartCol, b.StartCol )

        EndLine = System.Math.Max(a.EndLine, b.EndLine)
        EndCol = 
            if (a.EndLine > b.EndLine) then a.EndCol
            else if (a.EndLine < b.EndLine) then b.EndCol
            else System.Math.Min( a.EndCol, b.EndCol )
    }

/// Support for accessing the source token when building the parser value
let pmaps (p: Parser<'a, 's>) (f: ('a * SourceToken) -> 'b) : Parser<'b, 's> =
    fun (state, s) ->
        match run p state s with
        | Error e -> Error e
        | Ok (r, s', state) -> 
            Ok (f (r, makeToken s s'), s', state)

let peos : Parser<unit, _> =
    fun (state, s) ->
        match s.GetSafe(0) with
        | EOS -> Ok( (), s, state )
        | _ -> 
            err1 (s.pos) (Expected "end-of-file") state
            
/// Like |>> but also supplies the source token
let inline ( |>>> ) p f =
    pmaps p f

let singleSpaceTabEol =
    isAnyOf [ ' '; '\t'; '\n'; '\r' ]

let spaceOrTab : Parser<unit, 's> =
    let rec go (state, s: StringSegment) =
        match StringSegment.getSafe 0 s with
        | '\t' | ' ' -> go (state, s |> StringSegment.skip 1)
        | _ -> Ok ((), s, state)
    go

let eol : Parser<unit,_> = skipNewline // <|> (skipCr .>> skipNewline)

let ws = spaceOrTab

let isEol c = c = '\n' || c = '\r'
let isBlank = fun c -> c = ' ' || c = '\t'
let isBlankEol = fun c -> isBlank c || isEol c
let ws1 : Parser<_,UserState> = skipMany1SatisfyL isBlank "whitespace (ws1)"

//let comment : Parser<_,UserState> = pstring "//" >>. skipRestOfLine false

let keyword str : Parser<_,UserState> = pstring str //>>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str

let (<!?>) p label = p <?> label <!> label

let relOp : Parser<_,UserState> = 
    (keyword "<=" <|> keyword "<" <|> keyword "<>" <|> keyword ">=" <|> keyword ">" <|> keyword "=")
    <!?> "comparison"

let addOp : Parser<_,UserState> = keyword "+" <|> keyword "-"

let mulOp : Parser<_,UserState> = keyword "*" <|> (keyword "/" .>> (notFollowedByString "/"))

let ws_nl (nl_required : bool) : Parser<unit,UserState> =
    fun (state,input) ->
        let s' = input.SkipSpaceTab()
        let i = s'.GetOffsetToSkipNewline(0)

        if i = 0 then
            if nl_required then
                err1 (s'.pos) (Expected "newline") state
            else
                Ok ((), s', state )

        else
            let s'', ind = s'.SkipNewlineThenWhitespace( tabStopDistance, false )

            if ind < state.Indentation then
                err1 (s''.pos) (Expected "indentation") state
            else
                Ok( (), s'', { state with Indentation = ind } )

let wsbreak = ws_nl false

let wsbreaknl = ws_nl true
let wsb = wsbreak

let comment : Parser<unit,_> =
    fun (state, input) ->
        if input.GetSafe(0) = '/' && input.GetSafe(1) = '/' then
            let mutable i = 2
            let mutable foundEol=false
            while not foundEol do
                let c = input.GetSafe(i)
                match c with 
                | EOS ->
                    //i <- i - 1
                    foundEol <- true
                | '\r' ->
                    i <- i + 1
                    if (input.GetSafe(i) = '\n') then
                        i <- i + 1
                    foundEol <- true
                | '\n' ->
                    i <- i + 1
                    foundEol <- true
                | _ -> i <- i + 1
            
            Ok( (), input[i..], state )
        else 
            err1 input.pos (Expected "comment") state

let commentLine : Parser<unit,UserState> =
    ws >>. (comment <|> eol)

// Empty lines, or lines with comments
// Therefore, will not consume 
// - EOS
// - ws followed by EOS (ie, last line is ws with no EOL)
// - ws preceding valid progam character (non-ws)
//
let commentsWs =
    (many commentLine) .>> ws // consume trailing ws (next char will be EOS or program character)
    
let indentCurrentOrLower : Parser<unit, UserState> =  
    fun (state, input) ->
        let input', ind = input.SkipNewlineThenWhitespace( tabStopDistance, false )
        if ind <= state.Indentation then
            Ok ( (), input', state )
        else
            err1 (input.pos) (Expected (sprintf "indentation <= %d" state.Indentation)) state

let skipToNextWsb : Parser<unit, UserState> =
    fun (state, input) ->
        let mutable found = false
        let mutable input' = input

        while not found && input'.GetSafe(0) <> EOS do
            let atEol = input'.SkipToEol()

            let _, indent = atEol.SkipNewlineThenWhitespace( tabStopDistance, false )

            if indent >= 0 && indent <= state.Indentation then
                input' <- atEol
                found <- true
            else
                input' <- atEol.SkipEol()

        if found then
            Ok ( (), input', state )
        else
            err1 (input.pos) (Expected "Next statement") state

let breakable (p : Parser<'a,UserState>) : Parser<'a,UserState> =
    fun (state, input) ->
        let stateIndented  = { state with Indentation = input.startColumn; Errors = [] }

        // Log.log(sprintf "breakable: %d -> %d" state.Indentation stateIndented.Indentation)

        match run p stateIndented input with
        | Ok (v, input',  state') ->
            // Log.log(sprintf "breakable: Ok: %d -> %d" state'.Indentation state.Indentation)

            Ok (v, input', { state with Errors = state.Errors @ state'.Errors } )

        | Error (msgs,state') as e ->
            // Log.log(sprintf "breakable: Error: %d -> %d" state'.Indentation state'.Indentation)
            Error (msgs, { state' with Indentation = state.Indentation; Errors = state.Errors @ state'.Errors } )

let tolerant (p : Parser<'a,UserState>) (defaultValue : 'a) (skip : Parser<'b,UserState>): Parser<'a, UserState> =
    fun (state, input) ->
        match run p state input with
        | Ok _ as ok -> ok

        | Error (msgs, state) ->
            // Log.log (sprintf "tolerant: %A" (msgs |> List.toArray))
            // Log.log (sprintf "tolerant: input: '%s'" input.Value)
            // Log.log ("Skipping:")
            let msgs = filterMessages msgs

            let input' =
                match run skip state input with
                | Ok (_,s,_) -> 
                    //Log.log(sprintf "tolerant: skipped to '%s'" s.Value)
                    s
                | Error (msgs, _) -> 
                    //Log.log(sprintf "tolerant: skip failed: %A" (msgs |> List.toArray))
                    input

            Ok ( defaultValue, input', { state with Errors = state.Errors @ msgs })

let opt_unless (p1 : Parser<'a,UserState>) (p2 : Parser<'b,UserState>) : Parser< ('a * 'b) option, UserState> =
    fun (state, input) ->
        match run p1 state input with

        | Ok (v1, input', state') ->
            match run p2 state' input' with 
            | Ok (v2, input', state') -> 
                Ok( Some (v1,v2), input', state' )
            | Error (msgs, state) -> Error (msgs, state)

        | Error (msgs, state) ->
            Ok( None, input, state )

let binaryp  lhs op rhs fold 
    =
    //lhs .>>. (many (op .>>. rhs))
    lhs .>>. (opt_unless op rhs)
        |>> fun (e0,e1) -> match e1 with None -> e0 | Some opRhs -> fold e0 opRhs

let keywords = 
    [ 
    "in"; "fun"; "let"; "if"; "then"; "else"; "match"; "with"; "try"; "while"; "type"; "when"
    "as"; "module"; "rec"; "and"; "member"; "private"; "abstract"; "interface"; "end"; "do"; "true"; "false"
    ] |> Set

let notKeyword (p : Parser<string,_> ) : Parser<string,_> =
    fun (state,input) ->
        match run p state input with
        | Ok (v ,input', state') as ok -> 
            if keywords.Contains v then
                Error([ input.pos, [ Message ("Keyword not allowed: '" + v + "'") ] ], state' )
            else 
                ok
        | Error _ as e -> e


/// Use lookahead guards to enable parser to commit to a particular choice. This helps
/// with error reporting, and removes need to try any other parsers if that choice fails.
/// 
let choiceWithLookAheadL (label : string) (choices : (Parser<unit,'s> * Parser<'b,'s>) list) : Parser<'b,'s> =
    fun (state, s) ->
      let rec go state = function
        | [] -> err1 s.pos (Message "No parsers given") state

        | (peek,p) :: ps ->

          match run peek state s with
          | Ok _ -> 
                // We're committed to this parser now
                match run p state s with
                | Ok _ as ok -> ok
                | Error (errors, state) -> Error (errors, state)

          | Error (errors, state) -> 
                match ps with
                //| [] -> Error (errors @ errorsAcc, state)
                | [] -> err1 s.pos (Expected label) state
                | _ -> go state ps

      go state choices

let intconst : Parser<int * SourceToken,UserState> = 
    opt (pstring "-") .>>. many1Satisfy isDigit
    |>>> (fun ((s,digits),tok) -> ((s |> Option.defaultValue "") + digits) |> (stringToInt>>rtuple tok))

let pident0 : Parser<_,UserState> = letter <|> pchar '_'

let identifier : Parser<string,_> = 
    pident0 .>>. manySatisfy (fun p -> isDigit p || isAsciiLetter p || p = '_') |>> (fun (c,cs) -> string c + cs)

let name : Parser<string,_> =
    notKeyword identifier

let namet : Parser<_,UserState> =
    notKeyword identifier |>>> Name

let pcons = (keyword "::" .>> ws)

let (parseType : Parser<Type,_>), parseTypeRef = createParserForwardedToRef()
let (parseTypeTuple : Parser<Type,_>), parseTypeTupleRef = createParserForwardedToRef()
let (parseTypeLambda : Parser<Type,_>), parseTypeLambdaRef = createParserForwardedToRef()

let parseUnionType : Parser<Type,_> =
    (keyword "union<" <|> keyword "u<") .>> ws 
        >>. parseType .>> ws 
        .>> keyword "," .>> ws 
        .>>. parseType .>> ws 
        .>> keyword ">"
    |>> (fun (t1,t2) -> TyUnion (t1,t2))

let parseAnyType = (keyword "_" |>> (fun _ -> TyAny))

let parseTypeTerm : Parser<Type,UserState> =
    (keyword "num" |>> (fun _ -> TyNumber))
    <|> 
    parseAnyType
    <|> 
    (keyword "unit" |>> (fun _ -> TyUnit))
    <|>
    parseUnionType

let _parseTypeTuple : Parser<_,UserState> = 
    binaryp
        (parseTypeTerm .>> ws)
        (keyword "*" .>> ws)
        (parseTypeTuple .>> ws)
        (fun (lhs) (_,rhs) -> TyTuple(lhs, rhs))

let _parseTypeLambda : Parser<_,UserState> = 
    binaryp
        (parseTypeTuple .>> ws)
        (keyword "->" .>> ws)
        (parseTypeLambda .>> ws)
        (fun (lhs) (op,rhs) -> TyFunction(lhs, rhs))

let _parseType = parseTypeLambda

do  
    parseTypeTupleRef.Value <- _parseTypeTuple
    parseTypeLambdaRef.Value <- _parseTypeLambda
    parseTypeRef.Value <- _parseType

let parseCase =
    (parseUnionType <|> (parseAnyType |>> (fun _ -> TyUnion(TyAny,TyAny)))) .>> keyword "." .>>. (keyword "case1" <|> keyword "case2") .>> ws



let expect (expected : string) (p : Parser<'a,UserState>) : Parser<'a,UserState> =
    fun (state, input) ->
        match run p  state input with
        | Ok (result, input', state') ->
            Ok (result, input', state')
        | Error ( msgs, state' ) ->
            let best = filterMessages msgs
            Error ([ input.pos, [ Expected expected ] ] @ best , state' )