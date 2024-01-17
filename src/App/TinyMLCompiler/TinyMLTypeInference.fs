module TinyMLTypeInference

open TinyMLAst
open TypeInference
open Types

let mergec (c1:SymbolTypes) (c2:SymbolTypes) =
    (c1 |> Map.toList) @ (c2 |> Map.toList) |> Map

let rec private generate (ctx: TypingContext) e =
    match e with
    | Constant _ ->
        // NOTE: If the expression is a constant number, we return
        // its type (number) and generate no further constraints.
        TyNumber, [], Map.empty

    | ConstantB _ ->
        // NOTE: If the expression is a constant number, we return
        // its type (number) and generate no further constraints.
        TyBool, [], Map.empty

    | Binary(op, e1, e2) when op = "+" || op = "-" || op = "*" ->
        // NOTE: Recursively process sub-expressions, collect all the
        // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
        let t1, s1, syms1 = generate ctx e1
        let t2, s2, syms2 = generate ctx e2
        TyNumber, s1 @ s2 @ [ t1, TyNumber, tokenOf e1; t2, TyNumber, tokenOf e2 ], mergec syms1 syms2

    | Binary(">=", e1, e2) 
    | Binary("<=", e1, e2) 
    | Binary(">", e1, e2) 
    | Binary("<", e1, e2) 
    | Binary("<>", e1, e2) 
    | Binary("=", e1, e2) ->
        // TODO: Similar to the case for '+' but returns 'TyBool'
        // NOTE: Recursively process sub-expressions, collect all the
        // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
        let t1, s1, syms1 = generate ctx e1
        let t2, s2, syms2 = generate ctx e2
        TyBool, s1 @ s2 @ [ t1, TyNumber, tokenOf e1; t2, TyNumber, tokenOf e2 ], mergec syms1 syms2

    | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op

    | Variable (v,_) ->
        if not (ctx.ContainsKey v) then failwith ("Variable is not defined: " + v)
        // TODO: Just get the type of the variable from 'ctx' here.
        ctx[v], [], Map.empty

    | If(econd, etrue, efalse,_) ->
        let t1, s1, syms1 = generate ctx econd
        let t2, s2, syms2 = generate ctx etrue
        let t3, s3, syms3 = generate ctx efalse

        t2, s1 @ s2 @ s3 @ [ t1, TyBool, tokenOf econd; t3, t2, tokenOf efalse], syms1 |> mergec syms2 |> mergec syms3

    | Let( Name(v,_) as name, e1, e2_opt,_) ->
        let t1, s1, syms1 = generate ctx e1

        let syms = 
            match resolve t1 s1 with
            | Ok t -> syms1.Add(name, t) |> renameSyms 
            | _ -> syms1

        match e2_opt with
        | Some e2 ->
            let t2, s2, syms2 = generate (ctx.Add(v,t1)) e2
            t2, s1 @ s2, syms |> mergec syms2
        | None ->
            t1, s1, syms

    | Lambda( Name(v,_) as name, e) ->
        let targ = newTyVariable ()
        let t1, s1, syms1 = generate (ctx.Add(v,targ)) e

        let syms1 = 
            match resolve targ s1 with
            | Ok t -> syms1.Add(name, t)
            | _ -> syms1

        // TODO: We do not know what the type of the variable 'v' is, so we
        // generate a new type variable and add that to the 'ctx'. The
        // resulting type will be 'TyFunction' with 'targ' as argument type.
        TyFunction(targ,t1), s1, syms1

    | Application(e1, e2) ->
        let t1, s1, syms1 = generate ctx e1
        let t2, s2, syms2 = generate ctx e2
        let tresult = newTyVariable ()
        let ft = TyFunction( t2, tresult )
        // TODO: Tricky case! We cannot inspect the generated type of 'e1'
        // to see what the argument/return type of the function is. Instead,
        // we have to generate a new type variable and add a constraint.
        tresult, s1 @ s2 @ [ t1, ft, tokenOf e1 ], syms1 |> mergec syms2

    | Tuple(e1, e2) ->
        let t1, s1, syms1 = generate ctx e1
        let t2, s2, syms2 = generate ctx e2
        TyTuple (t1,t2), s1 @ s2, syms1 |> mergec syms2

    | TupleGet(b, e) ->
        // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
        // We need to generate two new type variables and a constraint.
        let t1, s1, syms1 = generate ctx e

        let v1 = newTyVariable ()
        let v2 = newTyVariable ()
        let tt = TyTuple( v1, v2 )

        match b with
        | true ->
            v1, s1 @ [ t1, tt, tokenOf e ], syms1
        | false ->
            v2, s1 @ [ t1, tt, tokenOf e ], syms1

    | Match(e, v1, e1, v2, e2) ->
        let te, s0, syms1 = generate ctx e

        let tv1 = newTyVariable ()
        let tv2 = newTyVariable ()
        let tu = TyUnion( tv1, tv2 )

        // TODO: As with tuples, we know the type of 'e' is some union,
        // but we do not know what. We need new type variables. When 
        // checking 'e1' and 'e2', add variable 'v' to the context!
        // Also note that the return types of 'e1' and 'e2S' have to match.
        let t1, s1, syms2 = generate (ctx.Add(v1,tv1)) e1
        let t2, s2, ctx3 = generate (ctx.Add(v2,tv2)) e2

        t1, s0 @ s1 @ s2 @ [ te, tu, tokenOf e; t1, t2, tokenOf e ], syms1 |> mergec syms2 |> mergec ctx3

    | Case(unionType, b, e) ->
        match unionType with
        | TyUnion (tcase1, tcase2) ->
            let t1, s1, syms1 = generate ctx e

            //let v1 = newTyVariable ()

            match b with 
            | true ->
                // let tu = TyUnion( t1, v1 )
                // tu, s1, syms1

                let tc1 = if tcase1 = TyAny then t1 else tcase1
                let tc2 = if tcase2 = TyAny then (newTyVariable()) else tcase2
                let tu = TyUnion ( tc1, tc2 )

                tu, s1 @ [t1, tc1, tokenOf e], syms1
            | false ->
                //let tu = TyUnion( v1, t1 )
                //tu, s1, syms1

                let tc1 = if tcase1 = TyAny then (newTyVariable())  else tcase1
                let tc2 = if tcase2 = TyAny then t1 else tcase2
                let tu = TyUnion ( tc1, tc2 )

                tu, s1 @ [t1, tc2, tokenOf e], syms1
        | _ ->
            raise (ParseException (Severity.Error, "Expected union type", tokenOf e))
        // TODO: Here, we know the type of 'e' is the type of one of 
        // the cases, but we still need a new type variable for the other.
        // failwith "not implemented"

    | Unit _ -> 
        // NOTE: This is so easy I wrote it for you :-)
        TyUnit, [], Map.empty

    | Recursive( Name(v,_) as name, e1, e2_opt, _) ->
        // TODO: This is easier than evaluation. We need a new type variable
        // for the type of the thing we are defining (variable 'v') and add
        // it to the context when checking both 'e1' and 'e2'.
        let v1 = newTyVariable()
        let t1, s1, syms1 = generate (ctx.Add(v,v1)) e1

        let syms = 
            match resolve t1 s1 with
            | Ok t ->
                syms1.Add(name, t) |> renameSyms
            | _ -> syms1

        match e2_opt with
        | Some e2 ->
            let t2, s2, syms2 = generate (ctx.Add(v,v1)) e2
            t2, s1 @ s2, syms |> mergec syms2
        | None ->
            t1, s1, syms

    | Block es ->
        match es with
        | [] -> TyUnit, [], Map.empty
        | xs -> 
            // TODO: warn if expressions prior to final expression have non-unit type
            generate ctx (List.last xs)

    | ConstantL es ->
        match es with
        | [] -> TyList (newTyVariable()), [], Map.empty
        | x::xs ->
            let t1, s1, syms1 = generate ctx x
            // TODO: Verify remaining elements of same type
            TyList t1, s1, syms1

    | Unary (op, e, tok) ->
        let t1, s1, syms1 = generate ctx e
        t1, s1 @ [ t1, TyNumber, tok], syms1

    // | ListMatch(e, v, e1, e2) -> 
    //     let tylist, s0 = generate ctx e

    //     let tyel = newTyVariable ()
    //     //let tylist = newTyVariable ()

    //     let t1, s1 = generate (ctx.Add(v,TyTuple(tyel,tylist))) e1
    //     let t2, s2 = generate (ctx.Add(v,TyUnit)) e2

    //     t1, s0 @ s1 @ s2 @ [ t1, t2; tylist, TyList tyel ]

    //     // TODO: Type of 'e' ('tylist') needs to be a list of elements ('tyel').
    //     // In 'e1', the type of the variable 'v' is then a tuple 'tyel * tylist'.
    //     // In 'e2', the type of the variable 'v' is just 'unit'.
    //     // To express this, you will need a new type variable for 'tyel'.

    // | ListCase(true, Tuple(ehd, etl)) -> 
    //     let t1, s1 = generate ctx ehd
    //     let t2, s2 = generate ctx etl
    //     // TODO: If type of 'ehd' is 'tyel' and type of 'etl' is 'tylist'
    //     // then we need a constraint 'tylist = list<tyel>'.
    //     TyList t1, s1 @ s2 @ [ t2, TyList t1 ]

    // | ListCase(false, Unit) -> 
    //     // TODO: The type of '[]' is a list of some type (needs a type variable)
    //     let v = newTyVariable()
    //     TyList v, []

    // | ListCase _ ->
    //     // TODO: For simplicity, we here restrict the syntax of list constructs.
    //     // In general, this is not needed, but it makes the task easier...
    //     failwith "unsupported list syntax"


let infer e = 
    // let g ctx e = 
    //     Log.log (sprintf "Infer: %A"  (TinyMLAst.mkAst e |> astToString))
    //     let t, cstr, syms = generate ctx e
    //     Log.log (sprintf "Context: %A"  
    //         (syms 
    //             |> Map.toArray 
    //             |> Array.map (fun (Name (a,_),b) -> sprintf "(%s: %A) " a b)))
    //     t,cstr,syms

    TypeInference.infer generate e
