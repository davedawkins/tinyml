module TinyMLEvaluator

open TinyMLAst
open Types

let toNum (n : bool) = (if n then 1 else 0) |> ValNum

let rec evaluate (ctx: VariableContext) e =
    match e with
    | ConstantL es ->
        es |> List.map (evaluate ctx) |> ValList
        
    | Block (es) ->
        es |> List.fold (fun _ e -> evaluate ctx e) ValUnit

    | Constant (n,_) -> 
        ValNum n

    | ConstantB (n,_) -> 
        ValBool n

    | Binary(op, e1, e2) ->
        let v1 = evaluate ctx e1
        let v2 = evaluate ctx e2

        match v1, v2 with
        | ValNum n1, ValNum n2 ->
            match op with
            | "+" -> ValNum(n1 + n2)
            | "-" -> ValNum(n1 - n2)
            | "*" -> ValNum(n1 * n2)
            | "=" -> toNum (n1 = n2)
            | "<>" -> toNum (n1 <> n2)
            | "<=" -> toNum (n1 <= n2)
            | ">=" -> toNum (n1 >= n2)
            | "<" -> toNum (n1 < n2)
            | ">" -> toNum (n1 > n2)
            | _ -> failwith "unsupported binary operator"
        | _ -> failwith "invalid argument of binary operator"

    | Variable (v,_) ->
        match ctx.TryFind v with
        | Some res -> res.Value
        | _ -> failwith ("unbound variable: " + v)

    // NOTE: You have the following from before

    | Unary(op, e,_) ->
        let v = evaluate ctx e

        match v with
        | ValNum n ->
            match op with
            | "-" -> ValNum(-n)
            | _ -> failwith "unsupported unary operator"
        | _ -> failwith "unary operations not supported for this type"

    | If(c, e1, e2, tok) ->
        let cv = evaluate ctx c

        match cv with
        | ValNum 1 -> evaluate ctx e1
        | _ -> evaluate ctx e2

    | Lambda( Name (v,_), e) -> ValClosure(v, e, ctx)

    | Application(e1, e2) ->
        let v1, v2 = (evaluate ctx e1), (evaluate ctx e2)

        match v1 with
        | ValClosure(v, e, ctx) -> evaluate (ctx.Add(v, lazy v2)) e
        | ValBuiltIn builtin -> builtin v2
        | _ -> failwith (sprintf "%A is not a function and cannot be applied" e1)

    | Let(v, e1, e2opt, _) -> 
        match e2opt with
        | Some e2 ->
            Application(Lambda(v, e2), e1) |> evaluate ctx
        | _ ->
            evaluate ctx e1 |> ignore
            ValUnit

    | Tuple(e1, e2) -> (evaluate ctx e1, evaluate ctx e2) |> ValTuple

    | TupleGet(b, e) ->
        match evaluate ctx e with
        | ValTuple(fst, snd) -> if b then fst else snd
        | v -> failwith (sprintf "Not a tuple: '%A'" v)

    | Match(e, v1, e1, v2, e2) ->
        match evaluate ctx e with
        | ValCase(b, cv) ->
            let v,matchCase = if b then v1,e1 else v2,e2
            evaluate (ctx.Add(v, lazy cv)) matchCase
        | x -> failwith (sprintf "Not case expression: '%A'" x)

    | Case(ut, b, e) -> ValCase(b, evaluate ctx e)

    | Recursive( Name(v,_), e1, e2, _) ->

        // let lambda = Lambda(v, e2)
        //let desugared =  Application( lambda, e1 )
        //evaluate ctx desugared

        let rec v1 = evaluate (ctx.Add(v, lazy v1)) e1

        match e2 with
        | Some e -> 
            evaluate (ctx.Add(v, lazy v1)) e
        | _ -> ValUnit

    // NOTE: This is so uninteresting I did this for you :-)
    | Unit _ -> ValUnit
