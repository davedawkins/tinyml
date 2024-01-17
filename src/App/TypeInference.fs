module TypeInference

open Types 

module Type =
    let rec collectVars (_this: Type) ( vars : Set<string> ) : Set<string> =
      match _this with
      | TyAny -> vars
      | TyUnit -> vars
      | TyVariable s -> vars.Add(s)
      | TyBool -> vars
      | TyNumber -> vars
      | TyList t -> collectVars t vars
      | TyUnion (a,r) -> collectVars a ( collectVars r vars ) 
      | TyFunction (a,r) -> collectVars a ( collectVars r vars ) 
      | TyTuple (a,b) -> collectVars a ( collectVars b vars )

    let rec renameUsing (_this: Type) ( mapping : Map<string,string> ) =
      match _this with
      | TyVariable s -> TyVariable (mapping[s])
      | TyList t -> TyList (renameUsing t mapping)
      | TyFunction (a,r) -> TyFunction (renameUsing a mapping, renameUsing r mapping)
      | TyTuple (a,b) -> TyTuple (renameUsing a mapping, renameUsing b  mapping)
      | TyUnion (a,b) -> TyUnion (renameUsing a mapping, renameUsing b mapping)
      | t -> t

    let generateMappingFrom ( allNames : Set<string> ) =
        let mapping = 
            Map.ofList 
                [ 
                    for i, n in Seq.indexed allNames ->
                        n, string('a' + char i) 
                ]
        mapping

    let generateMapping ( this : Type ) =
        collectVars this Set.empty |> generateMappingFrom

    let renameVars ( this : Type ) =
        generateMapping this |> renameUsing this

let rec occursCheck vcheck ty =
    match ty with
    | TyVariable s when s = vcheck -> true
    | TyList t -> occursCheck vcheck t
    | TyFunction (a,r) -> occursCheck vcheck a || occursCheck vcheck r
    | TyTuple (a,b) -> occursCheck vcheck a || occursCheck vcheck b
    | TyUnion (a,b) -> occursCheck vcheck a || occursCheck vcheck b
    | _ -> false

let rec substType (subst:Map<string, Type>) ty = 
    match ty with
    | TyVariable name ->
        subst.TryFind( name ) |> Option.defaultValue ty
    | TyList t ->
        TyList (substType subst t)
    | TyFunction (a,r) ->
        TyFunction (substType subst a,substType subst r)
    | TyTuple (a,b) ->
        TyTuple (substType subst a,substType subst b)
    | TyUnion (a,b) ->
        TyUnion (substType subst a,substType subst b)
    | _ -> ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type * SourceToken>) = 
    cs |> List.map (fun (t1,t2, tk) -> substType subst t1, substType subst t2, tk)

let substituteAll (defs : list<string * Type * SourceToken>) (t : Type) =
    defs |> List.fold (fun a (v,t',tk) -> substType (Map [v,t']) a) t

let rec solve (cs : List<Type*Type*SourceToken>): list<string*Type*SourceToken> =

    match cs with 
    | [] -> []

    | (_, TyAny, _)::constraints 
    | (TyAny, _, _)::constraints 
        -> solve constraints

    | (TyUnit, TyUnit, _)::constraints 
        -> solve constraints

    | (TyNumber, TyNumber, _)::constraints 
        -> solve constraints

    | (TyBool, TyBool, _)::constraints 
        -> solve constraints

    | (TyList t1, TyList t2, tok)::constraints 
        -> solve ((t1,t2, tok)::constraints)

    | (TyFunction(ta1, tb1), TyFunction(ta2, tb2), tok)::constraints 
        -> solve( [ta1, ta2, tok; tb1, tb2, tok]@constraints )

    | (TyTuple(ta1, tb1), TyTuple(ta2, tb2), tok)::constraints 
        -> solve( [ta1, ta2, tok; tb1, tb2, tok]@constraints )

    | (TyUnion(ta1, tb1), TyUnion(ta2, tb2), tok)::constraints 
        -> solve( [ta1, ta2, tok; tb1, tb2, tok]@constraints )

    | (t, TyVariable v, tok)::constraints 
    | (TyVariable v, t, tok)::constraints ->
        if occursCheck v t then failwith "Recursive definition"
        let constraints = substConstrs (Map [v, t]) constraints
        let subst = solve constraints
        let t = substituteAll subst t
        (v, t, tok)::subst
      
    | (a, b, tok)::_
        -> raise (ParseException( Severity.Error, (sprintf "Expected type '%A', but given '%A'" b a),tok))


// let _print (soln : List<string * Type>) = 
//     soln |> List.iter ( fun (v,t) -> printf "%s => %s" v (t.ToString()) ) 

let _solve constraints =
  try
    solve constraints |> Ok
  with
  | :? ParseException as x -> Error (x.Data0)
  | x -> Error (Severity.Error, sprintf "Failure: %s\n" x.Message, SourceToken.Empty)

type SymbolTypes = Map<Name,Type>
type TypingContext = Map<string, Type>

let newTyVariable =
    let mutable n = 0

    fun () ->
        n <- n + 1
        TyVariable(sprintf "_a%d" n)


let stripTokens triples = triples |> List.map (fun (t1,t2,_) -> t1,t2)

// Run both of the phases and return the resulting type
let resolve typ (constraints : List<Type*Type*SourceToken>) =
    match _solve constraints with

    | Ok subst ->
        typ |> substType (Map.ofList (stripTokens subst))  |> Ok

    | Error msg -> 
        Error msg

let renameSyms (m : Map<Name,Type> ) =
    let allvars = m.Values |> Seq.fold (fun set t -> Type.collectVars t set) Set.empty
    let mapping = Type.generateMappingFrom allvars

    m |> Map.map (fun _ t -> Type.renameUsing t mapping) 


let logSubst subst =
        Log.log(sprintf "solve: %A" (subst |> List.map (fun (a,b) -> sprintf "[%s := %A] " a b) |> Array.ofList))


let infer generate e =
    let sys = [ "print", TyFunction(TyAny,TyUnit) ] |> Map
    let typ, constraints, (syms : Map<Name,Type>) = generate sys e

//    let syms = syms.Add( Name("print", SourceToken.Empty), TyFunction(TyAny, TyUnit))   

    match _solve constraints with

    | Ok subst ->
        let typ = typ |> substType (Map.ofList (stripTokens subst)) |> Type.renameVars
        
        (typ , syms |> renameSyms) |> Ok

    | Error msg -> 
        Error msg

