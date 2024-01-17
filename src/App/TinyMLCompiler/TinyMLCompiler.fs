module TinyMLCompiler

open Types
open TinyMLAst

let private makeMsgFromParser (pos : Parsec.Position, et : Parsec.ErrorType ) =
    Severity.Error, et.ToString(), { StartLine = pos.Line; StartCol = pos.Col; EndLine = pos.Line; EndCol = pos.Col }

let private makeMsgFromMsgToken ( msg : string, tok : SourceToken ) =
    Severity.Error, msg, tok


module BuiltIns =

    let print (v : Value) =
        Log.log(sprintf "%A" v)
        ValUnit

    let alert( v : Value) =
        Log.log(sprintf "Alert: %A" v)
        ValUnit


type private CompiledObject( name : string, src : string, expr : Expression  )  =
    let _ast = mkAst expr

    let _sys = 
        [ "print", ( (TyFunction (TyAny, TyUnit)), ValBuiltIn BuiltIns.print)
          "alert", ( (TyFunction (TyAny, TyUnit)), ValBuiltIn BuiltIns.alert) 
          "version", ( (TyNumber), ValNum 1) 
          ]
        |> Map

    let _parentMap = 
        let rec pairs (_a : IAst) : seq<int*IAst> =
            _a.Children 
                |> Seq.collect (fun c -> [ (c.Id, _a ); yield! (pairs c) ])
        pairs _ast 
        |> Map

    let _expressionOf (a : IAst) =  (a :?> TinyMLAst.Ast ).Expression

    let _childExpressions (a : IAst) = a.Children |> Seq.map _expressionOf |> Seq.choose id

    let _typeSyms =
        try 
            match TinyMLTypeInference.infer expr with
            | Ok typeSyms -> Some typeSyms
            | Error _ -> None
        with _ -> None

    // Find Name instance that defines 'v'. Climb the syntax tree until we find a node 
    // owns that Name, by checking its _.Defines list

    let rec findDefinition (v : string) ( a : IAst ) : Name option=
        a.Defines 
        |> List.tryFind (fun (Name (n,_)) -> n = v) 
        |> Option.orElseWith (fun _ ->
            _parentMap.TryFind (a.Id) |> Option.bind (findDefinition v)
        )

    let getSymbolType (v : string) (a : IAst) =
        _typeSyms
        |> Option.bind (fun (_,syms) ->

            findDefinition v a |> Option.bind syms.TryFind 

        )
        |> Option.orElseWith (fun _ ->
            _sys.TryFind v |> Option.map fst
        )

    interface ICompiledObject with
        member _.Run() : Result<obj, string> =
            
            let ctx = _sys |> Map.map (fun _ (_,f) -> lazy f )

            try 
                let result = TinyMLEvaluator.evaluate ctx expr
                Ok result
            with
            | x -> 
                Error (x.Message)
        member _.Ast = _ast

        member _.TypeOf( a : IAst ) = 
            // Our label will be a symbol name, a keyword, or a literal
            getSymbolType a.Label a 

let private compile name src : Result<ICompiledObject, CompilerMessage list> =

    try 
        let compileResult = TinyMLParser.parse name src

        match compileResult with
        | Ok e ->
            let co = CompiledObject( name, src, e  ) :> ICompiledObject

            match TinyMLTypeInference.infer e with
            | Ok t ->
                Ok co
            | Error x -> Error [x]
            
        | Result.Error es ->
            es 
                |> List.collect (fun (p,m) -> m |> List.map (fun et -> makeMsgFromParser (p,et)))
                |> Result.Error
    with
    | :? ParseException as x -> Error [ x.Data0 ]
    | x -> Error [ Severity.Error, x.Message, SourceToken.Empty ]

let create() =
    { new ICompiler with 
        member _.Name = "TinyML"
        member _.Compile (name, src) = compile name src
    }
