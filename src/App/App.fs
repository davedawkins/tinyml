module App

open Sutil
open SutilOxide.FileSystem
open Types
open SutilOxide.Dock
open Sutil.Styling
open SutilOxide.Toolbar

open FileEditor
open MonacoEditor.Monaco.Editor
open MonacoEditor.Monaco.Languages
open MonacoEditor.Monaco
open Fable.Core

let [<Import("getMonarch","./monarch.js")>] getMonarch: unit -> MonacoEditor.Monaco.Languages.IMonarchLanguage = jsNative

let examples = [
    "compose.tml"
    "curry.tml"
    "datatypes.tml"
    "desugarargs.tml"
    "do.tml"
    "indent.tml"
    "let.tml"
    "list.tml"
    "match.tml"
    "partapply.tml"
    "print.tml"
    "README.md"
    "rec.tml"
    "tests.cfg" 
    "tuple.tml"
    "typecheck.tml"
    "union.tml"
]

type Context = {
    Dc : DockContainer
    Fs : IFileSystem
}

module Mvu = 
    type Model = {
        Files : string list
        FileIsModified : bool
        EditingFile : string
        FileLastSavedVersionId : float
        SelectedFile : string
        CompiledObject: ICompiledObject option
        CurrentAst : IAst option
        DebugEnabled : bool
        Compiler : ICompiler option
    }

    let init() = 
        { 
            FileIsModified = false; Files = []; SelectedFile = ""; EditingFile = ""
            CompiledObject = None; CurrentAst = None; DebugEnabled = false; FileLastSavedVersionId = -1
            Compiler = None
        }, Cmd.none


    let makeInfoMarker (msg : string, tok : SourceToken ) : MonacoEditor.Monaco.Editor.IMarkerData =
        {|
            message = msg
            severity = MonacoEditor.Monaco.MarkerSeverity.Info
            startLineNumber = tok.StartLine + 1
            startColumn = tok.StartCol+1
            endLineNumber = tok.EndLine + 1
            endColumn = tok.EndCol + 1           
        |} :> obj :?> MonacoEditor.Monaco.Editor.IMarkerData

    let makeMarker ( sev : Severity, msg : string, tok : SourceToken ) : MonacoEditor.Monaco.Editor.IMarkerData =
        {|
            message = msg
            severity = MonacoEditor.Monaco.MarkerSeverity.Error
            startLineNumber = tok.StartLine + 1
            startColumn = tok.StartCol+1
            endLineNumber = tok.EndLine + 1
            endColumn = tok.EndCol + 1           
        |} :> obj :?> MonacoEditor.Monaco.Editor.IMarkerData

    let clearMarkersOwnedBy( owner ) =
            FileEditor.editor.getModel()
                |> Option.iter (fun model ->
                    MonacoEditor.Monaco.editor.removeAllMarkers( owner ))

    let clearMarkers() =
        clearMarkersOwnedBy "Compiler"

    let addMarkers ms =
        FileEditor.editor.getModel()
            |> Option.iter (fun model ->
                MonacoEditor.Monaco.editor.setModelMarkers( model, "Compiler", ms |> List.map makeMarker |> Array.ofList  ))


    let addMarker owner m =
        FileEditor.editor.getModel()
            |> Option.iter (fun model ->
                MonacoEditor.Monaco.editor.setModelMarkers( model, owner, [| m |]  ))

    let loadExamples( fs : IFileSystem) = 
        promise {
            for file in examples do
                let! response = Fetch.fetch ("/examples/" + file) []
                let! content = response.text()

                fs.SetFileContent( "/" + file, content )
        }

    let okCancel (content : Sutil.Core.SutilElement) (confirm : unit -> unit) =
        SutilOxide.Modal.modal (
            { 
                SutilOxide.Modal.ModalOptions.Create() 
                    with
                        Buttons = [
                            "Cancel", (fun cancel -> cancel())
                            "OK", (fun cancel -> confirm(); cancel(); )
                        ]
                        Content = (fun close -> content)
            }
        )

    let askForName dispatch =
        let nameS = Store.make ""
        SutilOxide.Modal.modal (
            { 
                SutilOxide.Modal.ModalOptions.Create() 
                    with
                        Buttons = [
                            "Cancel", (fun cancel -> cancel())
                            "Create", (fun cancel -> dispatch (NewFile (nameS.Value)); cancel(); )
                        ]
                        Content = (fun close -> Html.div [
                            CoreElements.disposeOnUnmount [nameS]
                            Html.input [
                                Bind.attr("value", nameS)
                            ]
                        ])
            }
        )

    let rec update (ctx : Context) msg model = 
        let fs = ctx.Fs

        match msg with

        | ResetAllFiles confirmed ->
            let go d =
                loadExamples ctx.Fs |> Promise.start
            model, [go]

        | SetIsFileModified z ->
            { model with FileIsModified = z; CompiledObject = None }, Cmd.none

        | SetCompiler c ->
            let go d =
                Log.log("Compiler: '" + c.Name + "'")

            { model with Compiler = Some c}, [go]

        | SetDebugEnabled f ->
            let go d = 
                Log.log(sprintf "Debug: %s" (if f then "enabled" else "disabled") )
                ParserUtil.Debug.debugEnabled <- f
            { model with DebugEnabled = f }, [ go ]

        | SelectAst ast ->
            let go d =
                clearMarkersOwnedBy "Ast"
                addMarker "Ast" (makeInfoMarker (ast.Label,ast.Token))
            { model with CurrentAst = Some ast }, [go]

        | SetCompiledObject co ->
            { model with CompiledObject = co }, Cmd.none

        | SetEditingFile file ->
            { model with EditingFile = file }, Cmd.none

        | SetSelectedFile file ->
            { model with SelectedFile = file }, Cmd.none

        | SetFiles files ->
            { model with Files = files }, Cmd.none

        | RunTests ->
            match model.Compiler with
            | Some cc ->
                Tests.runTests cc (ctx.Fs.GetFileContent("tests.cfg")) |> Promise.start
            | None -> ()
            model, Cmd.none

        | AddMarkers ms -> 
            addMarkers ms
            model, Cmd.none

        | ClearMarkers -> 
            clearMarkers()
            model, Cmd.none

        | SaveFile name ->
            let versionId : float = 
                match name with
                | "" -> -1.0
                | nonEmptyFile ->
                    fs.SetFileContent(nonEmptyFile,FileEditor.editor.getValue())
                    FileEditor.getAlternativeVersionId()

            { model with FileIsModified = false; FileLastSavedVersionId = versionId }, Cmd.none

        | NewFile name ->
            let go dispatch =
                match name with
                | "" ->
                    askForName dispatch
                | nonEmptyName ->
                    fs.CreateFile("", nonEmptyName)
                    dispatch (OpenFile nonEmptyName)

            model, [ go ]

        | OpenFile file ->
            let go dispatch =
                if model.EditingFile <> "" then
                    dispatch (SaveFile model.EditingFile)
                FileEditor.edit FileEditor.editor file (fs.GetFileContent file)
                if file.EndsWith ".tml" then dispatch Compile
            { model with EditingFile = file; FileLastSavedVersionId = FileEditor.getAlternativeVersionId() }, [ go ]

        | ClearLog -> 
            Log.clear()
            model, Cmd.none

        | Run ->    
            match model.CompiledObject with
            | Some co -> 

                match co.Run() with
                | Ok value -> 
                    Log.log( sprintf "Program returned: %A\n" value)
                | Error msg -> 
                    Log.log( sprintf "Error: Program failed: %s\n" msg)
                    
            | None -> 
                Log.log ("Cannot run the program - successful compilation required first")
                ()
            model, Cmd.none

        | Compile ->    
            match model.Compiler, model.EditingFile with 
            | Some c, file when file.EndsWith ".tml" ->
                let go dispatch =
                    let src = (FileEditor.editor.getValue())
                    dispatch ClearMarkers
                    match c.Compile( file, src) with
                    | Ok result ->
                        Log.log( "Program compiled successfully")
                        dispatch (SetCompiledObject (Some result))
                    | Result.Error es ->
                        Log.log( "Program compilation failed")
                        es |> List.iter (fun (sev, msg, tok) -> Log.log(sprintf "[%d:%d] %s: %s" tok.StartLine tok.StartCol (string sev) msg))

                        dispatch (SetCompiledObject None)
                        dispatch ClearMarkers
                        es |> AddMarkers |> dispatch
                model, [ go ]
            | None, _ ->
                Log.log("No compiler selected!")
                model, Cmd.none
            | _, _ -> 
                Log.log("No file loaded!")
                model, Cmd.none

let viewFile (model : IStore<Mvu.Model>) dispatch (file : string) =
    Html.divc "file" [
        Bind.toggleClass( model |> Store.map (fun m -> m.SelectedFile = file), "selected")
        text file
        Ev.onDblClick (fun e -> dispatch (OpenFile file))
        Ev.onClick (fun e -> dispatch (SetSelectedFile file))
    ]


let rec findAstAt (ast : IAst) (line : int, col : int) : IAst list=
    
    let matches =
        if ast.Token.Contains(line,col) then
            //Log.log(sprintf "Match: (%d,%d) -> %s %A" line col ast.Label ast.Token)
            [ ast ]
        else
            []

    ast.Children 
        |> Seq.fold (fun m c -> (findAstAt c (line,col)) @ m ) matches


type TmlCodeLens() =
    interface  MonacoEditor.Monaco.Languages.CodeLensProvider with    
        member _.onDidChange with get() = Unchecked.defaultof<_> and set(v) = ()
        member _.provideCodeLenses(model, token) = 
            Unchecked.defaultof<_>
        member _.resolveCodeLens( model: ITextModel, codeLens: CodeLens, token: CancellationToken)
            = Unchecked.defaultof<_>

type TmlHoverProvider(hover : (Position -> string[] option)) =
    interface HoverProvider with
        member this.provideHover(model: ITextModel, position: Position, token: CancellationToken): ProviderResult<Hover> = 
            match hover position with
            | Some text ->
                let mkMdStr s = {| value = s |}
                let mdstrs = text |> Array.map mkMdStr

                U2.Case1( {| contents = mdstrs |} |> As<Hover> ) |> Some
            | None -> None

let hover (model : IStore<Mvu.Model>) (position : MonacoEditor.Monaco.Position)=
    match model.Value.CompiledObject with
    | Some co ->
        findAstAt co.Ast (int position.lineNumber - 1, int position.column - 1)
        |> List.choose (fun a -> co.TypeOf a |> Option.map (fun t -> t,a))
        |> List.map (fun (t,a) -> sprintf "%s: %s" a.Label (t.ToString()))
        |> Array.ofList
        |> Some
    | None ->
        None


let initMonaco model dispatch =
    FileEditor.registerLanguage( "tinyml", [ ".tml" ] )
    FileEditor.setMonarchTokensProvider("tinyml", getMonarch() ) 
    FileEditor.registerCodeLensProvider(  "tinyml", TmlCodeLens() ) 
    FileEditor.registerHoverProvider(  "tinyml", TmlHoverProvider(hover model) )
    FileEditor.onDidChangeModelContent( fun e -> 
        let fileIsModified = FileEditor.getAlternativeVersionId() <> model.Value.FileLastSavedVersionId

        if fileIsModified <> model.Value.FileIsModified  then
            dispatch (SetIsFileModified fileIsModified)
    )

let viewEditor context (model : IStore<Mvu.Model>) dispatch =
    Html.divc "editor wh100" [

        Ev.onKeyDown( fun ke -> 
            if ke.key = "s" && (ke.metaKey || ke.ctrlKey) then
                ke.preventDefault()
                dispatch (SaveFile model.Value.EditingFile) 
        )

        CoreElements.host (fun e ->
            FileEditor.initialise (e :?> Browser.Types.HTMLDivElement) 
            initMonaco model dispatch
        )
    ]

let viewLog context (model : IStore<Mvu.Model>) dispatch =
    Html.divc "messages wh100" [
        Bind.each( Log.messages, Log.viewMessage, fun m -> m.Id )
    ]

let viewFiles context (model : IStore<Mvu.Model>) dispatch =
    Html.divc "files wh100" [
        Bind.each( model .>> (_.Files), viewFile model dispatch, id )
    ]

let rec viewAstNode (model : IStore<Mvu.Model>) dispatch (co : ICompiledObject) (ast : IAst) =
    let isOpen = Store.make true

    let eqAst (other : IAst option) =
        match other with
        | None -> false
        | Some a -> ast.Id = a.Id

    Html.lic "ast-node" [

        let children = ast.Children |> Seq.toList
        let isEmpty = children.IsEmpty

        Html.span [

            Html.divc ("expand-button" + (if isEmpty then " empty" else "")) [
                Bind.toggleClass( isOpen, "expanded")
                if not isEmpty then Ev.onClick (fun _ -> isOpen |> Store.modify not)
            ]
            Html.spanc "ast-label" [
                Bind.toggleClass( model .> (fun m -> eqAst m.CurrentAst), "selected")
                text ast.Label
                match co.TypeOf ast with
                | None -> text ""
                | Some t -> Html.spanc "ast-type" [ text (" : " + t.ToString()) ]
            ]
            Ev.onClick (fun _ -> dispatch (SelectAst (ast)))
        ]

        yield!
            match children with
            | [] -> []
            | _ -> 
                [
                    Html.ulc "ast-children" [
                        Bind.toggleClass( isOpen, "expanded")
                        yield! (ast.Children |> Seq.map (viewAstNode model dispatch co))
                    ]
                ]
    ]

let viewAst context (model : IStore<Mvu.Model>) dispatch =
    Html.divc "ast wh100" [
        Bind.el( model .>> (_.CompiledObject), (fun co ->
            match co with
            | Some co -> viewAstNode model dispatch co co.Ast
            | None -> text "Compilation needed") 
        )
    ]

open type Feliz.length

let editModeButton dispatch current (compiler : ICompiler) =
    checkItem [
        ButtonProperty.Label compiler.Name
        IsChecked (current = Some compiler)
        OnCheckChanged (fun cheked -> 
            if cheked then dispatch (SetCompiler compiler)
        )
    ]

let requestRecycle model dispatch =
    Mvu.okCancel
        (Html.div [
            Html.div "This will reset *all* the example files to their original state!"
            Html.div "You can 'Cancel' and use 'Download' to save a copy of all files to your local device"
            Html.div "Are you sure want to proceed?"
        ])
        (fun _ -> dispatch (ResetAllFiles true))

let oxideToolbar context (model : IStore<Mvu.Model>) dispatch =
    Html.divc "header horizontal full-width gap" [
        toolbar [] [
            Attr.style [ Css.width (percent 100)]
            buttonItem [ ButtonProperty.Label "Clear";   ButtonProperty.Icon "fa-power-off"; OnClick (fun e -> dispatch ClearLog) ]

            if CompilerServices.Compilers.Length > 1 then
                Bind.el( model .> (fun m -> m.Compiler), fun current ->
                    dropDownItem [ ButtonProperty.Label "Compiler"] 
                        (CompilerServices.Compilers |> List.map (editModeButton dispatch current))
                    
                )

            buttonItem [ ButtonProperty.Label "Reset";   ButtonProperty.Icon "fa-recycle"; OnClick (fun e -> requestRecycle model dispatch) ]
            buttonItem [ ButtonProperty.Label "New";   ButtonProperty.Icon "fa-file-plus"; OnClick (fun e -> dispatch (NewFile "")) ]
            buttonItem [ ButtonProperty.Label "Download";   ButtonProperty.Icon "fa-file-export"; OnClick (fun e -> BlobUrl.downloadFs (context.Fs) "tinyml" |> Promise.start) ]
            buttonItem [ ButtonProperty.Label "Save";   ButtonProperty.Icon "fa-save"; OnClick (fun e -> dispatch (SaveFile model.Value.EditingFile)) ]
            buttonItem [ ButtonProperty.Label "Tests";   ButtonProperty.Icon "fa-hands-asl-interpreting"; OnClick (fun e -> dispatch RunTests ) ]
            buttonItem [ ButtonProperty.Label "Compile"; ButtonProperty.Icon "fa-sharp fa-light fa-gear-complex-code"; OnClick (fun e -> dispatch Compile) ]
            buttonItem [ ButtonProperty.Label "Run";   ButtonProperty.Icon "fa-play"; OnClick (fun e -> dispatch Run ) ]
            buttonItem [ ButtonProperty.Label "Debug";   ButtonProperty.Icon "fa-bug"; OnClick (fun e -> dispatch (SetDebugEnabled (not model.Value.DebugEnabled)) ) ]

            right [
                Attr.style [ Css.alignSelfCenter ]
            ]
        ]
    ]

let makeContext () =
    let dc = DockContainer({ Options.Create() with OnTabShow = fun (pane,show) -> () })
    let fs : IFileSystem = LocalStorageFileSystem "lang"

    let context = {
        Dc = dc
        Fs = fs
    }

    context

open SutilOxide.Types

let initPanes (context : Context) model dispatch (dc : DockContainer) =

    dc.AddPane( "Files", [
        Icon "fa-sharp fa-light fa-files"
        Location LeftTop
        Content (viewFiles context model dispatch)
        IsOpen true
    ])
    
    dc.AddPane( "Ast", [
        Icon "fa-sharp fa-light fa-files"
        Location RightTop
        Content (viewAst context model dispatch)
        IsOpen true
    ])
    
    dc.AddPane( "Log", [
        Icon "fa-light fa-message-lines"
        Location BottomLeft
        Content (viewLog context model dispatch)
        IsOpen true
        Header (
            Html.divc "horizontal" [
                text "Log"
                SutilOxide.Toolbar.vseparator
                SutilOxide.Toolbar.button "Clear" ("fa-light fa-power-off") (fun _ -> dispatch ClearLog)
            ]
        )
    ])

    let editorHeader() =
        Html.span [  
            Bind.el( model .>> (fun m -> sprintf "%s%s" m.EditingFile (if m.FileIsModified then " *" else "")), Html.span) 
        ]

    dc.AddPane( "Editor", [
        Icon "fa-sharp fa-light fa-code"
        Location CentreCentre
        Content (viewEditor context model dispatch)
        IsOpen true
        LabelEl (editorHeader())
        Header (editorHeader())
    ])

#if TINYML
    CompilerServices.Register( TinyMLCompiler.create() )
#endif
#if MELTE
    CompilerServices.Register( MelteCompiler.create() )
#endif //MELTE

    // Prefer TinyML, implementing the compiler outlined in Tomas's course
    CompilerServices.Compilers 
        |> List.tryFind (fun c -> c.Name = "TinyML") 
        |> Option.iter (dispatch<<SetCompiler)

let initFs (fs : IFileSystem) dispatch =

    FileEditor.onReady( fun _ ->
        promise {
            let files = fs.Files("/") |> Array.sort
            if (files.Length = 0) then
                do! Mvu.loadExamples fs
            dispatch (OpenFile "README.md")
            return ()
        } |> Promise.start
    )

    let rebuild() =
        fs.Files "" |> Array.sort |> List.ofArray |> SetFiles |> dispatch
    fs.OnChange (fun _ -> rebuild() )
    rebuild()

let view() =
    addGlobalStyleSheet (Browser.Dom.document) AppCss.style |> ignore
    SutilOxide.Css.installStyling AppCss.theme |> ignore

    let context = makeContext()
    let fs = context.Fs

    let model, dispatch = () |> Store.makeElmish Mvu.init (Mvu.update context) ignore

    initFs fs dispatch

    Html.divc "vertical container thm-control" [
        oxideToolbar context model dispatch
        Html.divc "main horizontal full-width full-height gap" [
            context.Dc.View (initPanes context model dispatch)
        ]
    ]

view() |> Sutil.Program.mount

ParserUtil.Debug.log <- Log.log

Log.log "Initialized"
