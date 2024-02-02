module TinyMLAst
// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

open Types

type TExpression = (Expression * SourceToken)
and Expression =
    | Block of TExpression list
    | ConstantB of bool
    | Constant of int
    | ConstantL of TExpression list
    | Binary of string * TExpression * TExpression
    | Unary of string * TExpression
    | If of TExpression * TExpression * TExpression
    | Application of TExpression * TExpression
    | Lambda of Name * TExpression
    | Let of Name * TExpression * TExpression option
    | Tuple of TExpression * TExpression
    | TupleGet of bool * TExpression
    | Case of Type * bool * TExpression
    | Match of TExpression * string * TExpression * string * TExpression
    | Recursive of Name * TExpression * TExpression option
    | Variable of string 
    | Unit

and Value =
    | ValNum of int
    | ValBool of bool
    | ValClosure of string * TExpression * VariableContext
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

let mkBlock (es : TExpression list) tok =
    match es with
    | [ e ] -> e
    | _ -> Block es, tok

let mergeSourceTokens a b =
    {
        File = a.File
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

let tokenOf ( (_,t): 'a * SourceToken ) = t

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

let rec mkAst ( (expr,_t) as tolerantExpr : TExpression) : IAst =

    let _mkast ls = ls |> List.map mkAst
    let _expr = Some expr

    match expr with
    | Block (es) -> Ast("Do",  _t, _expr, es |> _mkast, [])
    | ConstantL es -> Ast("List", _t, _expr, es |> _mkast, [])
    | Binary (v ,e1 ,e2) -> Ast(v, _t, _expr, [ e1; e2 ] |> _mkast, [])
    | Unary (v ,e1 ) -> Ast(v, _t, _expr, [ mkAst e1 ],[])
    | If (c ,e1 ,e2) -> Ast("If", _t, _expr, [ c; e1; e2 ] |> _mkast,[])
    | Application (f , a) -> Ast("Apply", _t, _expr, [ f; a ] |> _mkast,[])
    | Lambda ( Name(v,vt) as name ,body) -> 
        Ast("Lambda",  _t, _expr, 
            [ 
                Ast(v, vt, None, [],[] ) 
                mkAst body
            ],[name])
    | Let ( Name(v,vt) as name  , (e1,e1_token) ,e2_option) -> 
        Ast("Let", _t, _expr,
             [ 
                Ast(v, vt, Some e1, [],[])
                mkAst (e1, e1_token)
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
    | Recursive ( Name(v,vt) as name ,(e1, e1_token) ,e2_option) -> 

        Ast("Rec", _t, _expr,
             [ 
                Ast(v, vt, Some e1, [],[])
                mkAst (e1, e1_token)
                yield! 
                    match e2_option with 
                    | None -> []
                    | Some e2 -> [ mkAst e2 ]
            ], [name]
        )
    | Constant (n) -> Ast(string n, _t, _expr, [],[])
    | ConstantB (n) -> Ast(string n, _t, _expr, [],[])
    | Variable v -> Ast(v, _t, _expr, [],[])
    | Unit -> Ast("()", _t, _expr, [],[])