module ParserUtil

open Parsec
open Types

let tabStopDistance = 8 // must be a power of 2

type StreamState<'T>() = class end

type StringSegment with

    member this.AtEOS = this.GetSafe(0) = Parsec.EOS

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
                i <- i + 1
                match c with 

                | '/' when this.GetSafe(i) = '/' ->
                    i <- i + 1
                    let mutable inComment = true
                    while inComment do
                        let c = this.GetSafe(i)
                        i <- i + 1
                        match c with 
                        | EOS -> inComment <- false
                        | '\n' ->
                            ind <- 0
                            inComment <- false
                        | '\r' ->
                            if this.GetSafe(i) = '\n' then i <- i + 1
                            ind <- 0
                            inComment <- false
                        | _ -> ()

                | ' ' -> 
                    ind <- ind + 1
                | '\t' -> 
                    let d = tabStopDistanceMinus1 + 1 - (ind &&& tabStopDistanceMinus1);
                    ind <- (ind + d)
                | '\f' when allowFf -> 
                    ind <- 0
                | '\r'->
                    if this.GetSafe(i) = '\n' then i <- i + 1
                    ind <- 0
                | '\n'->
                    ind <- 0
                | _ ->
                    i <- i - 1 
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

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        (fun (state,stream) ->

            if debugEnabled then 
                sprintf "%s%A: Entering '%s': '%s'" (_spaces()) stream.pos label (stream.Value |> escapeStr) |> log
                debugIndent <- debugIndent + 1

            let result = (p <?> label) (state,stream)

            if debugEnabled then 
                debugIndent <- debugIndent - 1
                sprintf "%s%A: Leaving '%s' (%A)" (_spaces()) stream.pos label (result.ToString() |> escapeStr) |> log

            result)

open Debug

type UserState = 
    {
        Indentation: int
        IsTopLevel : bool
        JsxTags : string list
    }
    with
        static member Create() = { Indentation = -1; IsTopLevel = false; JsxTags = [] }
        //member this.Indentation = match this.IndentationStack with [] -> -1 | x::_ -> x

let console = Fable.Core.JS.console

let stringToInt (s : string) : int = System.Int32.Parse s

let tuple a b = (a,b)
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

let isBlank = fun c -> c = ' ' || c = '\t'

let ws1 : Parser<_,UserState> = skipMany1SatisfyL isBlank "whitespace (ws1)"

let comment : Parser<_,UserState> = pstring "//" >>. skipRestOfLine false

let keyword str : Parser<_,UserState> = pstring str //>>? nextCharSatisfiesNot (fun c -> isLetter c || isDigit c) <?> str

let relOp : Parser<_,UserState> = 
    (keyword "<=" <|> keyword "<" <|> keyword "<>" <|> keyword ">=" <|> keyword ">" <|> keyword "=")
    <!> "comparison"

let addOp : Parser<_,UserState> = keyword "+" <|> keyword "-"

let mulOp : Parser<_,UserState> = keyword "*" <|> keyword "/"

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

let breakable (p : Parser<'a,UserState>) : Parser<'a,UserState> =
    fun (state, input) ->
        let stateIndented  = { state with Indentation = input.startColumn }
        match run p stateIndented input with
        | Ok (v, input', _) ->
            Ok (v, input',state)
        | Error _ as e -> e

let binaryp  lhs op rhs fold 
    =
    lhs .>>. (many (op .>>. rhs))
        |>> fun (e0,es) -> es |> List.fold fold e0

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
                Error([ input'.pos, [ Message ("Keyword not allowed: '" + v + "'") ] ], state' )
            else 
                ok
        | Error _ as e -> e

let choiceWithLookAhead (choices : (Parser<unit,'s> * Parser<'b,'s>) list) : Parser<'b,'s> =
    fun (state, s) ->
      let rec go state errorsAcc = function
        | [] -> err1 s.pos (Message "No parsers given") state

        | (peek,p) :: ps ->

          match run peek state s with
          | Ok _ -> 
                // We're committed to this parser now
                match run p state s with
                | Ok _ as ok -> ok
                | Error (errors, state) -> Error (errors @ errorsAcc, state)

          | Error (errors, state) -> 
                match ps with
                | [] -> Error (errors @ errorsAcc, state)
                | _ -> go state (errors @ errorsAcc) ps

      go state [] choices


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

