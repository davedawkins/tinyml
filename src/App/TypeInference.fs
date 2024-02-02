module TypeInference

open Types 

type SymbolTypes = Map<Name,TType>

type TypingContext = Map<string, TypeScheme>


module Type =
    let rec collectVars (_this: TType) ( vars : Set<string> ) : Set<string> =
      match typeOf _this with
      | TyAny -> vars
      | TyUnit -> vars
      | TyVariable s -> vars.Add(s)
      | TyBool -> vars
      | TyNumber -> vars
      | TyList t -> collectVars t vars
      | TyUnion (a,b) -> collectVars a ( collectVars b vars ) 
      | TyFunction (a,r) -> collectVars a ( collectVars r vars ) 
      | TyTuple (a,b) -> collectVars a ( collectVars b vars )

    let rec renameUsing (_this: TType) ( mapping : Map<string,string> ) : TType=
      let tk = snd _this
      match typeOf _this with
      | TyVariable s -> TyVariable (mapping[s]), tk
      | TyList t -> TyList (renameUsing t mapping), tk
      | TyFunction (a,r) -> TyFunction (renameUsing a mapping, renameUsing r mapping), tk
      | TyTuple (a,b) -> TyTuple (renameUsing a mapping, renameUsing b  mapping), tk
      | TyUnion (a,b) -> TyUnion (renameUsing a mapping, renameUsing b mapping), tk
      | t -> _this

    let generateMappingFrom ( allNames : Set<string> ) =
        let mapping = 
            Map.ofList 
                [ 
                    for i, n in Seq.indexed allNames ->
                        n, string('a' + char i) 
                ]
        mapping

    let generateMapping ( this : TType ) =
        collectVars this Set.empty |> generateMappingFrom

    let renameVars ( this : TType ) =
        generateMapping this |> renameUsing this

let contextToStr (ctx:TypingContext) =
    ctx |> Seq.map (fun kv -> sprintf "%s:%s" kv.Key (kv.Value.ToString()))
        |> String.concat ", "

let constrToString (cstr) =
    (cstr 
        |> List.map (fun (t1,t2) -> sprintf "%s :: %s" (t1.ToString()) (t2.ToString()))
        |> String.concat ", "
        )

let substToString (subst:Map<string, Type>) =
    subst 
        |> Map.toArray 
        |> Array.map (fun (s,t) -> sprintf "%s / %s" s (t.ToString()))
        |> String.concat ", "

let rec occursCheck vcheck (ty : TType) =
    match typeOf ty with
    | TyVariable s when s = vcheck -> true
    | TyList t -> occursCheck vcheck t
    | TyFunction (a,r) -> occursCheck vcheck a || occursCheck vcheck r
    | TyTuple (a,b) -> occursCheck vcheck a || occursCheck vcheck b
    | TyUnion (a,b) -> occursCheck vcheck a || occursCheck vcheck b
    | _ -> false

let rec substType (subst:Map<string, TType>) ty = 
    let tyTok = snd ty
    match typeOf ty with
    | TyVariable name ->
        subst.TryFind( name ) |> Option.defaultValue ty
    | TyList t ->
        TyList (substType subst t),tyTok
    | TyFunction (a,r) ->
        TyFunction (substType subst a,substType subst r), tyTok
    | TyTuple (a,b) ->
        TyTuple (substType subst a,substType subst b),tyTok
    | TyUnion (a,b) ->
        TyUnion (substType subst a,substType subst b),tyTok
    | _ -> ty
    // |> fun r ->
    //     Log.log( sprintf "substType: %s => %s using %s" (ty.ToString()) (r.ToString()) (substToString subst))
    //     r


let rec freeTypeVars (t : TType) =
    match typeOf t with
    | TyVariable n -> [ n ] |> Set
    | TyList t -> freeTypeVars t
    | TyUnion (t1,t2)
    | TyTuple (t1,t2)
    | TyFunction (t1,t2) -> freeTypeVars t1 |> Set.union (freeTypeVars t2)
    | _ -> Set.empty

let freeTypeVarsInScheme ( TypeScheme (vars,t) : TypeScheme) =
    Set.difference (freeTypeVars t) (Set vars)

let freeTypeVarsInContext (ctx : TypingContext) =
    ctx.Values 
        |> Seq.map freeTypeVarsInScheme
        |> Seq.fold Set.union Set.empty

let generalize (ctx : TypingContext) (t : TType) : TypeScheme =
    let vars =  Set.difference (freeTypeVars t) (freeTypeVarsInContext ctx)
    TypeScheme (vars |> Set.toList, t)

let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_%d" n)

let instantiate ( TypeScheme (vars,t) : TypeScheme) : TType =
    let subs = vars |> List.map (fun v -> v, (newTyVariable(), snd t))
    substType (Map subs) t

let tokOf (tt: TType) = snd tt

let substConstrs (subst:Map<string, TType>) (cs:list<TType * TType>) = 
    cs |> List.map (fun (t1,t2) -> substType subst t1, substType subst t2)

let substituteAll (defs : list<string * TType >) (t : TType) =
    defs |> List.fold (fun a (v,t') -> substType (Map [v,t']) a) t

let rec solve (cs : List<TType*TType>): list<string*TType> =
    match cs with 
    | [] -> []

    | (_, (TyAny, _))::constraints 
    | ( (TyAny,_), _)::constraints 
        -> solve constraints

    | ( (TyUnit,_), (TyUnit, _))::constraints 
        -> solve constraints

    | ( (TyNumber,_), (TyNumber, _))::constraints 
        -> solve constraints

    | ( (TyBool,_),(TyBool, _))::constraints 
        -> solve constraints

    | ( (TyList t1, tok), ((TyList t2), _))::constraints 
        -> solve (( (t1), t2 )::constraints)

    | ( (TyFunction(ta1, tb1), tok), (TyFunction(ta2, tb2), _))::constraints 
        -> solve( [ (ta1), ta2; (tb1), tb2]@constraints )

    | ( (TyTuple(ta1, tb1),tok), (TyTuple(ta2, tb2), _))::constraints 
        -> solve( [(ta1), ta2; (tb1), tb2]@constraints )

    | ((TyUnion(ta1, tb1),tok), (TyUnion(ta2, tb2), _))::constraints 
        -> solve( [(ta1), ta2; (tb1), tb2]@constraints )

    | (t, (TyVariable v,tok))::constraints 
    | ((TyVariable v,tok), t)::constraints ->
        if occursCheck v t then 
            raise (ParseException( Severity.Error, (sprintf "Recursive definition: %s : %s" v (t.ToString())),tok))
        let constraints = substConstrs (Map [v, t]) constraints
        let subst = solve constraints
        let t = substituteAll subst t
        (v, t)::subst
      
    | ( (a, atok), (b, btok))::_
        -> raise (ParseException( Severity.Error, (sprintf "Expected type '%A', but given '%A'" a b),btok))

    |> (fun result ->
        result
    )

// let _print (soln : List<string * Type>) = 
//     soln |> List.iter ( fun (v,t) -> printf "%s => %s" v (t.ToString()) ) 

let _solve constraints =
  try
    solve constraints |> Ok
  with
  | :? ParseException as x -> Error (x.Data0)
  | x -> Error (Severity.Error, sprintf "Failure: %s\n" x.Message, SourceToken.Empty)


// Run both of the phases and return the resulting type
let  resolve typ (constraints : List<TType*TType>) =
    match _solve constraints with

    | Ok subst ->
//        Log.log( sprintf "resolve: subst: %A" (stripTokens subst |> Array.ofList))
        let typ1 = typ |> substType (Map.ofList subst)  

//        Log.log( sprintf "substType: %s => %s using %s" (typ.ToString()) (typ1.ToString()) (substToString (subst |> stripTokens |> Map)) )

        Ok typ1

    | Error msg -> 
        Error msg

let renameSyms (m : Map<Name,TType> ) =
    let allvars = m.Values |> Seq.fold (fun set t -> Type.collectVars t set) Set.empty
    let mapping = Type.generateMappingFrom allvars

    m |> Map.map (fun _ t -> Type.renameUsing t mapping) 


let logSubst subst =
        Log.log(sprintf "solve: %A" (subst |> List.map (fun (a,b) -> sprintf "[%s := %A] " a b) |> Array.ofList))


let infer generate e =
    let ctx : TypingContext = Map.empty
    let sys = [ "print", generalize ctx (TyFunction( Type.NoSource TyAny, Type.NoSource TyUnit), SourceToken.Empty) ] |> Map
    let typ, constraints, (syms : Map<Name,TType>) = (generate sys e)

    match _solve constraints with

    | Ok subst ->
        let typ = typ |> substType (Map.ofList subst) |> Type.renameVars
        
        (typ , syms |> renameSyms) |> Ok

    | Error msg -> 
        Error msg

