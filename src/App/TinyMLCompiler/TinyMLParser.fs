module TinyMLParser

open Parsec
open ParserUtil
open TinyMLAst
open Types
open ParserUtil.Debug

let reduceLeft (pair : (Expression * Expression) -> Expression) (list : Expression list) =
    let rec go xs =
        match xs with
        | [ e ] -> e
        | [ e1; e2 ] -> pair (e1,e2)
        | e1::e2::es-> go( pair(e1,e2) :: es )
        | [] -> failwith "Unexpected empty expression list"
    (go list)

let (expr : Parser<Expression,UserState>), exprRef = createParserForwardedToRef()
let binaryMulExpr, binaryMulExprRef = createParserForwardedToRef()
let binaryAddExpr, binaryAddExprRef = createParserForwardedToRef()
let binaryRelExpr, binaryRelExprRef = createParserForwardedToRef()
let (blockExpr : Parser<Expression,UserState>), blockExprRef = createParserForwardedToRef()

let intexpr : Parser<_,UserState> = 
    intconst |>> Constant

let constant : Parser<_,UserState> = 
    intexpr    

// Fixme: >>= is made for doing this
let variable : Parser<Expression,_> = 
    notKeyword identifier|>>> Variable

let ifthenelse : Parser<Expression,UserState> =
    breakable (
        (keyword "if" .>> wsb) >>.
        (expr .>> wsb) .>>
        (keyword "then" .>> wsb) .>>.
        (expr .>> wsb) .>>
        (keyword "else"  .>> wsb) .>>.
        expr 
    )
    |>>> (fun (((a,b),c), tok) -> If(a,b,c,tok))

let caseOf =
    breakable (
        (keyword "|" .>> ws) >>. 
        name .>> ws .>>
        (keyword "->" .>> wsb) .>>. 
        expr
    )

let matchwith : Parser<Expression,UserState> =
    breakable (
        (keyword "match" .>> ws) >>.
        (expr .>> ws) .>>
        (keyword "with" .>> wsb) .>>.
        caseOf  .>> wsb .>> ws .>>.
        caseOf
    )
    |>> (fun ((e,(v1,e1)),(v2,e2)) -> Match (e,v1,e1,v2,e2))
    <!> "match"
    
let letb = 
    breakable (
        (keyword "let" .>> wsb)
        >>. (opt (keyword "rec" .>> wsb ))
        .>>. namet .>> ws
        .>>. (many (namet .>> ws) ) .>> ws
        .>>  (wsb >>. (keyword "=" )) 
        .>>. (wsb >>. (expr <!> "expression for binding"))
        .>>. opt_unless (wsb >>. keyword "in" .>> wsb) ((expect "expression for 'in'" (expr <!> "'in'-expression")) <!> "expect in-expression")
    )
    |>>> (fun x -> 
        let (((((r,name),args),e1),e2),t) = x

        let rec curry args e =
            match args with
            | [] -> e
            | a::xs -> Lambda(a, curry xs e)

        let e1_curried = curry args e1

        let e2 = e2 |> Option.map snd

        match r with
        | Some _ -> Recursive (name, e1_curried, e2,t)
        | _ -> Let (name, e1_curried, e2,t)
        )

let lambda = 
    (keyword "fun" <!> "'fun' keyword")
    >>. ws1  
    //>>. namet
    >>. (many (namet .>> ws) ) .>> ws
    .>> (ws >>. (keyword "->" <!> "'->' operator")) 
    .>>. (ws >>. (expr <!> "value for binding")) 

    |>>> (fun ((args,expr),tok) ->

        let rec curry args e =
            match args with
            | [] -> e
            | a::xs -> Lambda(a, curry xs e)

        curry args expr)

    <!> "lambda"

let subexpr = 
    (keyword "()" |>>> (fun (_,t) -> Unit t)) <|>
    ((keyword "(" .>> ws >>. expr .>> ws .>> keyword ")"))
    <!> "subexpr"

let block = 
    breakable <|
        (keyword "do") .>> wsbreak >>. blockExpr

let listexpr : Parser<Expression,UserState> = 
    between (keyword "[" .>> ws) (keyword "]") (sepBy (expr .>> ws) (keyword ";" .>> ws) )
    |>> ConstantL

let term, termRef = createParserForwardedToRef()

let parseCaseValue =
    parseCase .>>. term |>>>
        (fun (((t,caseId),expr), tok)-> 
            match t with
            | TyUnion (t1,t2) -> 
                match caseId with
                | "case1" -> Expression.Case( t, true, expr )
                | "case2" -> Expression.Case( t, false, expr )
                | _ -> raise (ParseException ( Severity.Error, "Expected 'case1' or 'case2'", tok))
            | _ ->
                raise (ParseException ( Severity.Error, "Expected union<'a,'b>", tok))
        )

let _term : Parser<_,UserState> =
    choiceWithLookAheadL "term" [
        followedBy (keyword "true"), (keyword "true" |>>> (fun (_,t) -> ConstantB (true,t) ))
        followedBy (keyword "false"), (keyword "false" |>>> (fun (_,t) -> ConstantB (false,t) ))
        followedBy (keyword "do"), (expect "block" block)
        followedBy (keyword "("), (expect "subexpression" subexpr)
        followedBy (keyword "["), (expect "list" listexpr)
        followedBy (keyword "union" <|> keyword "u<" <|> keyword "_.case"), (expect "case value" parseCaseValue)
        followedBy (satisfy (fun c -> c = '-' || isDigit c)), (expect "number" constant)
        followedBy (pident0), (expect "variable" variable)
    ]

termRef.Value <- _term

let tupleget =
    term .>>. (opt (keyword "#" >>. (pchar '1' <|> pchar '2'))) .>> ws 
    |>> (fun (e,c) -> 
            match c with 
            | None -> e 
            | Some dig -> TupleGet( (dig = '1') , e )
        )

let application = 
    (many1 (tupleget .>> ws) |>> (reduceLeft Application)) <!> "application"

let factor = 
    choiceWithLookAheadL "factor" [
        followedBy (keyword "fun"), (expect "lambda" lambda) <!> "lambda"
        followedBy (keyword "if"), (expect "if-then-else" ifthenelse) <!> "ifthenelse"
        followedBy (keyword "match"), matchwith <!> "matchwith"
        followedBy (keyword "let"), (expect "let binding" letb) <!> "letb"
        followedBy (term), application
    ]

let tolerantOp op : Parser<string,UserState> =
    tolerant op "?" (skipManySatisfy (fun c -> not (isBlank c)))

let tolerantExpr expr : Parser<Expression,UserState> =
    tolerant
        expr
        (Unit SourceToken.Empty)
        (skipManySatisfy (not<<isBlankEol) .>> ws)


let _binaryMulExpr =
    breakable <|
        binaryp 
            (factor .>> ws) 
            (mulOp .>> ws) 
            ((expect "expression for rhs" binaryMulExpr) .>> ws) 
            (fun e1 (op,e2) -> Binary(op,e1,e2))

let _binaryAddExpr =
    binaryp 
        (binaryMulExpr .>> ws) 
        (addOp .>> ws) 
        (binaryAddExpr .>> ws) 
        (fun e1 (op,e2) -> Binary(op,e1,e2))

let _binaryRelExpr =
    binaryp 
        (binaryAddExpr .>> ws) 
        (relOp .>> ws) 
        (binaryRelExpr .>> ws) 
        (fun e1 (op,e2) -> Binary(op,e1,e2))

let tupleExpr = 
    breakable (
        (binaryRelExpr .>> ws) .>>.
        opt (keyword "," >>. ws >>. binaryRelExpr .>> ws) 
    ) |>> (fun (e1,b) -> match b with Some e2 -> Tuple(e1,e2) | _ -> e1)
    <!> "tuple"

let blockExprSeparator =
    wsbreak .>> notFollowedBy (eol <|> eof)
    <!> "blockExprSeparator"

let exprItem = 
    tolerant 
        expr
        (Unit SourceToken.Empty)
        skipToNextWsb
        //(skipManyTill skipAnyChar indentCurrentOrLower)

let _blockExpr =
    breakable ((sepBy1 (exprItem <!?> "expr-item") blockExprSeparator) <!> "expr-sequence")
        |>> (fun es -> match es with [e] -> e | _ -> mkBlock (es))
        <!> "block-expression"

let _expr =
    tupleExpr <!> "expression"

do
    exprRef.Value <- _expr
    binaryAddExprRef.Value <- (_binaryAddExpr <!> "binary add")
    binaryMulExprRef.Value <- (_binaryMulExpr <!> "binary mul")
    binaryRelExprRef.Value <- (_binaryRelExpr <!> "binary rel")
    blockExprRef.Value <- _blockExpr

let trailingwseol : Parser<_,UserState> =
    (many (ws >>. eol >>. ws)) <!> "trailingwseol"

let rec unflattenLets (es : Expression list) : Expression list =
    match es with

    | [] ->  []

    | [ Recursive (_,_,None, t) ] 
    | [ Let (_,_,None, t)  ] -> 
        raise (ParseException ( Severity.Error, "Final 'let' block has no 'in' expression to provide a value", t))

    | Recursive (v,e1,None, t) :: xs  ->
        [ Recursive( v, e1, Some <| mkBlock (unflattenLets xs),t ) ]

    | Recursive (v,e1,Some e2, t) :: xs  ->
        [ Recursive( v, e1, Some <| mkBlock (e2 :: unflattenLets xs), t ) ]

    | Let (v,e1,None, t) :: xs  -> 
        [ Let( v, e1, Some <| mkBlock (unflattenLets xs), t ) ]

    | Let (v,e1,Some e2, t) :: xs  -> 
        [ Let( v, e1, Some <| mkBlock (e2 :: unflattenLets xs), t ) ]

    | x :: xs -> 
        x :: unflattenLets xs

let unflattenExpr (e : Expression) =
    match e with
    | Block (es) -> (unflattenLets es) |> mkBlock
    | _ -> e

let punitexpr : Parser<Expression, UserState> = preturn (Unit (SourceToken.Empty))

let program =
    blockExpr .>> 
    commentsWs .>> 
    peos

let document : Parser<Expression,_> =
    commentsWs >>.

    // Allow empty file to be a valid program, of type 'unit'
    choiceWithLookAheadL "program" [
        (followedBy peos) <!> "check: EOS", preturn (Unit (SourceToken.Empty))
        (preturn ()) <!> "check: program", program
    ] 

    <!> "document"

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

let parse (name: string) (src: string) : Expression * (Position * ErrorMessage) list =
    match runString document (UserState.Create()) src with
    | Ok (value,_,state) ->
        (unflattenExpr value, state.Errors)
    | Error (msgs,_) ->
        (Unit (SourceToken.Empty), msgs)

