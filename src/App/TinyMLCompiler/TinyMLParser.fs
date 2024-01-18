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
    notKeyword identifier |>>> Variable

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
        .>>. opt (
                (wsb >>. keyword "in") >>. (wsb >>. expr)
        ) <!> "expression for let"
    )
    |>>> (fun x -> 
        let (((((r,name),args),e1),e2),t) = x

        let rec desugar args e =

            match args with
            | [] -> e
            | a::xs -> Lambda(a, desugar xs e)

        let e1_desugared = desugar args e1

        match r with
        | Some _ -> Recursive (name, e1_desugared, e2,t)
        | _ -> Let (name, e1_desugared, e2,t)
        )

let lambda = 
    (keyword "fun" <!> "'fun' keyword")
    >>. (ws1  >>. (namet <!> "argument")) 
    .>> (ws >>. (keyword "->" <!> "'->' operator")) 
    .>>. (ws >>. (expr <!> "value for binding")) 
    |>> Lambda
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
    choiceWithLookAhead [
        followedBy (keyword "true"), (keyword "true" |>>> (fun (_,t) -> ConstantB (true,t) ))
        followedBy (keyword "false"), (keyword "false" |>>> (fun (_,t) -> ConstantB (false,t) ))
        followedBy (keyword "do"), block
        followedBy (keyword "("), subexpr
        followedBy (keyword "["), listexpr
        followedBy (keyword "union" <|> keyword "u<" <|> keyword "_.case"), parseCaseValue
        followedBy (satisfy (fun c -> c = '-' || isDigit c)), constant
        followedBy (pident0), variable
    ] <!> "term"

termRef.Value <- _term

let application = 
    (many1 (term .>> ws) |>> (reduceLeft Application)) <!> "application"

let factor = 
    choiceWithLookAhead [
        followedBy (keyword "fun"), lambda <!> "lambda"
        followedBy (keyword "if"), ifthenelse <!> "ifthenelse"
        followedBy (keyword "match"), matchwith <!> "matchwith"
        followedBy (keyword "let"), letb <!> "letb"
        followedBy (term), application
    ] <!> "factor"

let tupleget =
    factor .>>. (opt (keyword "#" >>. (pchar '1' <|> pchar '2'))) .>> ws 
    |>> (fun (e,c) -> 
            match c with 
            | None -> e 
            | Some dig -> TupleGet( (dig = '1') , e )
        )

let _binaryMulExpr =
    binaryp 
        (tupleget .>> ws) 
        (mulOp .>> ws) 
        (binaryMulExpr .>> ws) 
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

let _blockExpr =
    breakable (sepBy1 expr blockExprSeparator)
        |>> (fun es -> match es with [e] -> e | _ -> mkBlock (es))

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

let document : Parser<Expression,_> =
    //
    // blockExprSeparator is easy way to consume comments at start of file
    //
    (opt blockExprSeparator) >>. 
    blockExpr .>> 
    (ws >>. trailingwseol) 
    <!> "document"
        

let parse (name: string) (src: string) : Result<Expression,_> =
    match runString document (UserState.Create()) src with
    | Ok (value,_,_) ->
        Result.Ok (unflattenExpr value)
    | Error (msgs,_) ->
        Error (msgs)

