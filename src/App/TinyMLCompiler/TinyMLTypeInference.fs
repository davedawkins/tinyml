module TinyMLTypeInference

open TinyMLAst
open TypeInference
open Types

let mergec (c1:SymbolTypes) (c2:SymbolTypes) =
    (c1 |> Map.toList) @ (c2 |> Map.toList) |> Map


let mutable indent = 0

let _debug = false


let rec private generateDebug (ctx : TypingContext) (e) : TType * ( (TType * TType) list) * SymbolTypes = 
    if _debug then
        Log.log (sprintf "%sEnv {%A} |- %s"
            (String.replicate (indent * 8) " ")
            (contextToStr ctx)
            (TinyMLAst.mkAst e |> astToString)
        )

    indent <- indent + 1
    let t, (cstr : List<TType*TType>), syms = generate ctx e
    indent <- indent - 1

    if _debug then
        Log.log (sprintf "%s-| C{%s}"  
            (String.replicate (indent * 8) " ")
            (constrToString  cstr)
        )

    t,cstr,syms

and private generate (ctx: TypingContext) (e, etok) : TType * List<TType * TType> * SymbolTypes =

    let _g = if _debug then generateDebug else generate
    let wantSyms = true

    match e with
    | Constant _ ->
        (TyNumber, etok), [], Map.empty

    | ConstantB _ ->
        (TyBool, etok), [], Map.empty

    | Binary(op, e1, e2) when op = "+" || op = "-" || op = "*" ->
        let t1, s1, syms1 = _g ctx e1
        let t2, s2, syms2 = _g ctx e2
        (TyNumber, etok), s1 @ s2 @ [ t1, (TyNumber, tokenOf e1); t2, (TyNumber, tokenOf e2) ], mergec syms1 syms2

    | Binary(">=", e1, e2) 
    | Binary("<=", e1, e2) 
    | Binary(">", e1, e2) 
    | Binary("<", e1, e2) 
    | Binary("<>", e1, e2) 
    | Binary("=", e1, e2) ->
        let t1, s1, syms1 = _g ctx e1
        let t2, s2, syms2 = _g ctx e2
        (TyBool, etok), s1 @ s2 @ [ t1, (TyNumber, tokenOf e1); t2, (TyNumber, tokenOf e2) ], mergec syms1 syms2

    | Binary(op, e1, _) ->
        raise (ParseException(Severity.Error, (sprintf "Binary operator '%s' not supported." op), tokenOf(e1)))
        
    | Variable (v) ->
        if not (ctx.ContainsKey v) then raise (ParseException(Severity.Error, ("Variable is not defined: " + v), etok))
        (instantiate ctx[v]), [], Map.empty

    | If(econd, etrue, efalse) ->
        let t1, s1, syms1 = _g ctx econd
        let t2, s2, syms2 = _g ctx etrue
        let t3, s3, syms3 = _g ctx efalse

        let t4 = newTyVariable()

        t2, s1 @ s2 @ s3 @ [ t1, (TyBool, tokenOf t1); t2, (t4, tokenOf(t2)); t3, (t4, tokenOf t3)], syms1 |> mergec syms2 |> mergec syms3

    | Let( Name(v,_) as name, e1, e2_opt) ->
        let t1, s1, syms1 = _g ctx e1

        let ts1 = 
            match resolve t1 s1 with
            | Ok t -> generalize ctx t
            | Error m -> raise (ParseException(m))

        let syms = 
            if wantSyms then
                match ts1 with
                | TypeScheme (_,t) -> 
                    syms1.Add(name, t) |> renameSyms 
            else
                Map.empty

        match e2_opt with
        | Some e2 ->
            let t2, s2, syms2 = _g (ctx.Add(v, ts1)) e2
            t2, s1 @ s2, syms |> mergec syms2
        | None ->
            t1, s1, syms

    | Recursive( Name(v,vtok) as name, e1, e2_opt) ->
        let v1 = newTyVariable()
        let t1, s1, syms1 = _g (ctx.Add(v, TypeScheme ([], (v1,vtok)) )) e1

        let ts1 = 
            match resolve t1 s1 with
            | Ok t -> generalize (ctx.Add(v, TypeScheme ([],(v1,vtok)))) t
            | Error m -> raise (ParseException(m))

        let syms = 
            if wantSyms then
                match ts1 with
                | TypeScheme (_,t) -> syms1.Add(name, t) |> renameSyms 
            else
                Map.empty

        match e2_opt with
        | Some e2 ->
            let t2, s2, syms2 = _g (ctx.Add(v, ts1)) e2
            t2, s1 @ s2, syms |> mergec syms2
        | None ->
            t1, s1, syms

    | Lambda( Name(v,vtok) as name, e) ->

        let targ = newTyVariable ()
        let t1, s1, syms1 = _g (ctx.Add(v, TypeScheme ([],(targ,vtok)) )) e

        let syms1 = 
            if wantSyms then
                match resolve (targ,vtok) s1 with
                | Ok t -> 
                    syms1.Add(name, t)
                | _ -> 
                    syms1
            else
                Map.empty

        (TyFunction( (targ,vtok), t1), etok), s1, syms1

    | Application(e1, e2) ->
        let t1, s1, syms1 = _g ctx e1
        let t2, s2, syms2 = _g ctx e2
        let tresult = newTyVariable ()
        let ft = TyFunction( t2, (tresult, snd e2) )
        (tresult, etok), s1 @ s2 @ [ t1, (ft,etok) ], syms1 |> mergec syms2

        // ('a*'a->'a) : (Num -> 'x)
        // ('a*'a) : Num ; 'a : 'x 
    | Tuple(e1, e2) ->
        let t1, s1, syms1 = _g ctx e1
        let t2, s2, syms2 = _g ctx e2
        (TyTuple (t1,t2), etok), s1 @ s2, syms1 |> mergec syms2

    | TupleGet(b, e) ->
        let t1, s1, syms1 = _g ctx e

        let v1 = newTyVariable ()
        let v2 = newTyVariable ()
        let tt = TyTuple( (v1, tokenOf e), (v2, tokenOf e) )

        match b with
        | true ->
            (v1,etok), s1 @ [ t1, (tt, tokenOf e) ], syms1
        | false ->
            (v2,etok), s1 @ [ t1, (tt, tokenOf e) ], syms1

    | Match(e, v1, e1, v2, e2) ->
        let te, s0, syms1 = _g ctx e

        let tv1 = newTyVariable ()
        let tv2 = newTyVariable ()
        let tu = TyUnion( (tv1, tokenOf e), (tv2, tokenOf e) ), etok

        // TODO: As with tuples, we know the type of 'e' is some union,
        // but we do not know what. We need new type variables. When 
        // checking 'e1' and 'e2', add variable 'v' to the context!
        // Also note that the return types of 'e1' and 'e2S' have to match.
        let t1, s1, syms2 = _g (ctx.Add(v1, generalize ctx (tv1, tokenOf e))) e1
        let t2, s2, ctx3 = _g (ctx.Add(v2, generalize ctx (tv2, tokenOf e))) e2
        let tr = newTyVariable()

        t1, s0 @ s1 @ s2 @ [ te, tu; t1, (tr, tokenOf t1); t2, (tr, tokenOf t2) ], syms1 |> mergec syms2 |> mergec ctx3

    | Case(unionType, b, e) ->
        match unionType with
        | TyUnion (tcase1, tcase2) ->
            let t1, s1, syms1 = _g ctx e

            //let v1 = newTyVariable ()

            match b with 
            | true ->
                // let tu = TyUnion( t1, v1 )
                // tu, s1, syms1

                let tc1 = if typeOf tcase1 = TyAny then t1 else tcase1
                let tc2 = if typeOf tcase2 = TyAny then (newTyVariable(), tokenOf tcase2) else tcase2
                let tu = TyUnion ( tc1, tc2 )

                (tu, etok), s1 @ [t1, tc1 ], syms1
            | false ->
                //let tu = TyUnion( v1, t1 )
                //tu, s1, syms1

                let tc1 = if typeOf tcase1 = TyAny then (newTyVariable(), tokenOf tcase1)  else tcase1
                let tc2 = if typeOf tcase2 = TyAny then t1 else tcase2
                let tu = TyUnion ( tc1, tc2 )

                (tu, etok), s1 @ [t1, tc2 ], syms1
        | _ ->
            raise (ParseException (Severity.Error, "Expected union type", tokenOf e))

    | Unit -> 
        (TyUnit, etok), [], Map.empty

    | Block es ->
        match es with
        | [] -> (TyUnit, etok), [], Map.empty
        | xs -> 
            // TODO: warn if expressions prior to final expression have non-unit type
            _g ctx (List.last xs)

    | ConstantL es ->
        match es with
        | [] -> (TyList (newTyVariable(), etok), etok), [], Map.empty
        | x::xs ->
            let t1, s1, syms1 = _g ctx x
            // TODO: Verify remaining elements of same type
            (TyList t1, etok), s1, syms1

    | Unary (op, e) ->
        let t1, s1, syms1 = _g ctx e
        let t1k = TyNumber, tokenOf e
        t1, s1 @ [ t1, t1k ], syms1

let infer e = 
    TypeInference.infer generate e
