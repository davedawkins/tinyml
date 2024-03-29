module Types


type SourceToken = 
    {
        File : string
        StartLine : int; StartCol : int
        EndLine : int; EndCol : int
    }
    with 
    static Empty = { File = ""; StartLine = 0; StartCol = 0; EndLine = 0; EndCol = 0 }
    member __.Contains ( line, col ) =
        (line = __.StartLine && col >= __.StartCol) && (line = __.EndLine && col <= __.EndCol) ||
        (line > __.StartLine && line < __.EndLine)

type TType = Type * SourceToken

and Type =
    | TyVariable of string
    | TyBool
    | TyNumber
    | TyList of TType
    | TyFunction of TType * TType
    | TyTuple of (TType * TType)
    | TyUnion of (TType * TType)
    | TyUnit
    | TyAny
  with 
    static NoSource (t : Type) : TType = t, SourceToken.Empty
    override this.ToString() =
      match this with
      | TyAny -> "any"
      | TyUnit -> "unit"
      | TyUnion ( (a, _) , (b, _) ) ->
            sprintf "case<%s,%s>" (a.ToString()) (b.ToString())

      | TyTuple ((a, _) , (b, _) ) ->
            sprintf "(%s * %s)" (a.ToString()) (b.ToString())
      
      | TyFunction ((a, _) , (r, _) ) -> 
            // sprintf "(%s -> %s)" (a.ToString()) (r.ToString())
            match a with 
            | TyFunction _ ->
                sprintf "(%s) -> %s" (a.ToString()) (r.ToString())
            | _ ->
                sprintf "%s -> %s" (a.ToString()) (r.ToString())
      | TyVariable s -> sprintf "'%s" s
      | TyBool -> "bool"
      | TyNumber -> "num"
      | TyList t -> sprintf "list<%s>" (t.ToString())

type TypeScheme = TypeScheme of string list * TType
    with
        override __.ToString() =
            match __ with TypeScheme (vars, t) -> 
                sprintf "forall %A . %s" (vars |> Array.ofList) (t.ToString())

type QualifiedName = 
    {
        Name : string
        Path : string []
    }


type Name = Name of (string * SourceToken)


let typeOf (tt : TType) = fst tt

[<RequireQualifiedAccess>]
type Severity = Error | Warning | Info

type CompilerMessage = Severity * string * SourceToken

// A node in the syntax tree. Each compiler will create its own mapping from its Expression
// to an instance of an IAst.
// This class is useful as a way of generalizing the syntax tree, and providing a hierarchy.

type IAst =
    abstract Id : int
    abstract Label: string 
    abstract Token: SourceToken
    abstract Children: seq<IAst>
    abstract Type : Result<Type,string>
    abstract Defines : Name list

let rec astToString (ast : IAst) : string =
    "("
        + 
            ([ 
                ast.Label
                yield! (ast.Children |> Seq.map astToString)
            ] |> String.concat " ")
        + ")"

let newAstId = 
    let mutable n = 0
    fun () ->
        n <- n + 1
        n

// A general purpose Ast node
type Ast( label : string, token : unit -> SourceToken, children : seq<IAst> ) =
    let _id = newAstId()
    interface IAst with
        member _.Id = _id
        member _.Label = label
        member _.Token with get() = token()
        member _.Children = children
        member _.Type = Error ""
        member _.Defines = []

type ICompiledObject =
    abstract Run: unit -> Result<obj,string>
    abstract Ast: IAst
    abstract TypeOf : IAst -> TType option

type ICompiler =
    abstract Name : string
    abstract Compile: name:string * src:string  -> Result<ICompiledObject, CompilerMessage list>

type CompilerServices =
    static let mutable compilers : ICompiler list = []

    static member Register( c : ICompiler ) =
        compilers <- compilers @ [ c ]

    static member Compilers = compilers

type Message = 
    | AddMarkers of CompilerMessage list
    | RunTests
    | ClearMarkers
    | ClearLog
    | Compile
    | Run
    | OpenFile of string
    | OpenFileOnly of string
    | NewFile of string
    | SaveFile of string
    | SetFiles of string list
    | SetEditingFile of string
    | SetSelectedFile of string
    | SetCompiledObject of ICompiledObject option
    | SelectAst of IAst
    | SetDebugEnabled of bool
    | SetCompiler of ICompiler
    | SetIsFileModified of bool
    | ResetAllFiles of bool

exception ParseException of CompilerMessage

let inline As<'T>( x : obj ) = x :?> 'T
