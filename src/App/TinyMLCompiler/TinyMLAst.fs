module TinyMLAst
// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

open Types

type Expression =
    | Block of Expression list
    | ConstantB of bool * SourceToken
    | Constant of int * SourceToken
    | ConstantL of Expression list
    | Binary of string * Expression * Expression
    | Unary of string * Expression * SourceToken
    | If of Expression * Expression * Expression * SourceToken
    | Application of Expression * Expression
    | Lambda of Name * Expression
    | Let of Name * Expression * Expression option * SourceToken
    | Tuple of Expression * Expression
    | TupleGet of bool * Expression
    | Case of Type * bool * Expression
    | Match of Expression * string * Expression * string * Expression
    | Recursive of Name * Expression * Expression option * SourceToken
    | Variable of string * SourceToken
    | Unit of SourceToken

and Value =
    | ValNum of int
    | ValBool of bool
    | ValClosure of string * Expression * VariableContext
    | ValTuple of Value * Value
    | ValList of Value list
    | ValCase of bool * Value
    | ValUnit
    | ValBuiltIn of (Value -> Value)
    with
    override this.ToString() =
        match this with
        | ValBool b -> if b then "true" else "false"
        | ValNum n -> string n
        | ValClosure (v, e, c) -> sprintf "fun %s -> %A" v e
        | ValTuple (a,b) -> sprintf "(%A,%A)" a b
        | ValCase (b,v) -> sprintf "case%d %A" (if b then 1 else 2) v
        | ValUnit -> "()"
        | ValList v -> 
            sprintf "%A" (v)
        | ValBuiltIn _ -> "(builtin)"

and VariableContext = Map<string, Lazy<Value>>

let mkBlock es =
    match es with
    | [ e ] -> e
    | _ -> Block es

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

let rec tokenOf (e : Expression) =
    let notoken e = failwith (sprintf "No token found for %A" e)
    match e with
    | Match (e,_,_,_,_) -> tokenOf e
    | Tuple (e1,_) -> tokenOf e1
    | TupleGet (_,e1) -> tokenOf e1
    | Unit t -> t
    | Variable (_,t) -> t
    | Constant (_,t) -> t
    | ConstantB (_,t) -> t
    | Binary (_,e1,e2) -> mergeSourceTokens (tokenOf e1) (tokenOf e2)
    | Let (_,_,_,t) -> t
    | Recursive (_,_,_,t) -> t
    | Lambda (_, e) -> tokenOf e
    | Application (f, a) -> mergeSourceTokens (tokenOf f) (tokenOf a)
    | If (_,_,_,t) -> t
    | Case (_,_,e) -> tokenOf e
    | Block es ->
        match es with
        | [ e ] -> tokenOf e 
        | _ -> SourceToken.Empty
    | _ -> notoken e

(*
let rec _template_match (e : Expression) =

    match e with
    | Block es -> ""
    | ConstantL es -> ""
    | Binary (v ,e1 ,e2) -> ""
    | Unary (v ,e , t) -> ""
    | If (c ,e1 ,e2) -> ""
    | Application (f ,e) -> ""
    | Lambda (v ,e) -> ""
    | Let (v ,e1 ,e2_option , t) -> ""
    | Tuple (e1 ,e2) -> ""
    | TupleGet (bool ,e) -> ""
    | Case (bool ,e) -> ""
    //| Match (Expression * v ,e ,e
    | Match (e , cases) -> ""
    | Recursive (v ,e1 ,e2_option , t) -> ""
    | Constant (n , t) -> ""
    | Variable (v , t) -> ""
    | Unit (t) -> ""
    | Jsx (e) -> ""
    | TypeDef (td) -> ""
    *)


type Ast( label : string, tok : SourceToken, e : Expression option, children : seq<IAst>, defines : Name list ) =
    let _id = newAstId()
    member _.Expression = e
    interface IAst with
        member _.Id = _id
        member _.Label = label
        member _.Token with get() = tok
        member _.Children = children
        member _.Type = Error ""
        member _.Defines = defines

let rec mkAst (expr : Expression) : IAst =
    let _t = tokenOf expr 
    let _mkast ls = ls |> List.map mkAst
    let _expr = Some expr

    match expr with
    | Block (es) -> Ast("Do",  _t, _expr, es |> _mkast, [])
    | ConstantL es -> Ast("List", _t, _expr, es |> _mkast, [])
    | Binary (v ,e1 ,e2) -> Ast(v, _t, _expr, [ e1; e2 ] |> _mkast, [])
    | Unary (v ,e1 , t) -> Ast(v, _t, _expr, [ mkAst e1 ],[])
    | If (c ,e1 ,e2, t) -> Ast("If", _t, _expr, [ c; e1; e2 ] |> _mkast,[])
    | Application (f , a) -> Ast("Apply", _t, _expr, [ f; a ] |> _mkast,[])
    | Lambda ( Name(v,vt) as name ,body) -> 
        Ast("Lambda",  _t, _expr, 
            [ 
                Ast(v, vt, None, [],[] ) 
                mkAst body
            ],[name])
    | Let ( Name(v,vt) as name  ,e1 ,e2_option , t) -> 
        Ast("Let", _t, _expr,
             [ 
                Ast(v, vt, Some e1, [],[])
                mkAst e1
                yield! 
                    match e2_option with 
                    | None -> []
                    | Some e2 -> [ mkAst e2 ]
            ],[name]
        )
    | Tuple (e1 ,e2) -> Ast("Tuple", _t, _expr, [e1;e2] |> _mkast,[] )
    | TupleGet (b ,e1) -> Ast("TupleGet '" + (string b) + "'",  _t, _expr, [ mkAst e1 ],[])
    | Case (ut, b ,e1) -> Ast("Case '" + (string b) + "'",  _t, _expr, [ mkAst e1],[])
    | Match (me, v1, c1, v2,c2) -> Ast("Match", _t, _expr, 
            [me;c1;c2] |> _mkast,[]
        )
    | Recursive ( Name(v,vt) as name ,e1 ,e2_option , t) -> 

        Ast("Rec", _t, _expr,
             [ 
                Ast(v, vt, Some e1, [],[])
                mkAst e1
                yield! 
                    match e2_option with 
                    | None -> []
                    | Some e2 -> [ mkAst e2 ]
            ], [name]
        )
    | Constant (n , t) -> Ast(string n, _t, _expr, [],[])
    | ConstantB (n , t) -> Ast(string n, _t, _expr, [],[])
    | Variable (v , t) -> Ast(v, _t, _expr, [],[])
    | Unit (t) -> Ast("()", _t, _expr, [],[])