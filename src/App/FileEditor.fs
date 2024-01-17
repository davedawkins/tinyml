module FileEditor

// Wrappers for Monaco just for quality of life elsewhere in the application. A lot of the Typescript API peculiarities
// make for some jaggy boilerplate.
// There should be nothing application-specific in this module.

open Fable.Core.JsInterop
open Fable.Core

open MonacoEditor
open MonacoEditor.Monaco
open Monaco.Languages

let inline As<'T>( x : obj ) = x :?> 'T

importSideEffects ("./monaco-vite.js")

let [<Import("getMonarch","./monarch.js")>] getMonarch: unit -> MonacoEditor.Monaco.Languages.IMonarchLanguage = jsNative

let mutable private modelMap: Map<string, MonacoEditor.Monaco.Editor.ITextModel> = Map.empty
let mutable editor = Unchecked.defaultof<_>
let mutable private editorReady = false
let mutable private readyListeners : (unit -> unit) list = []

let onReady (ready : unit -> unit) =
    if editorReady then ready() 
    else readyListeners <- readyListeners @ [ ready ]

let private setEditor (e : MonacoEditor.Monaco.Editor.IStandaloneCodeEditor) = 

    editor <- e
    editorReady <- true
    readyListeners |> List.iter (fun f -> f())
    readyListeners <- []

let private fileNameToLanguagePairs = 
    [
        ".json", "json"
        ".fs", "fsharp"
        ".js", "typescript"
        ".jsx", "typescript"
        ".ts", "typescript"
        ".tsx", "typescript"
        ".html", "html"
        ".md", "markdown"
        ".css", "css"
    ]

let mutable fileNameToLanguage = Map fileNameToLanguagePairs 

let getAlternativeVersionId() =
    editor.getModel() |> Option.map (fun e -> e.getAlternativeVersionId()) |> Option.defaultValue 0

let registerCodeLensProvider ( lang, provider ) =
    Monaco.languages.registerCodeLensProvider( U3.Case1 lang, provider ) |> ignore

let registerHoverProvider( lang, provider ) =
    Monaco.languages.registerHoverProvider( U3.Case1 lang, provider ) |> ignore

let setMonarchTokensProvider( lang, provider ) =
    Monaco.languages.setMonarchTokensProvider(lang, U2.Case1 (provider) ) |> ignore

let onDidChangeModelContent( handler ) =
    onReady( fun _ ->
        editor.onDidChangeModelContent.Invoke( fun e -> handler(e); None ) |> ignore
    )

let registerLanguage( id : string, extensions : string list ) =
    Monaco.languages.register( {|
        id = id
        extensions = extensions |> Array.ofList
    |} |> As<ILanguageExtensionPoint> )

    fileNameToLanguage <- extensions |> List.fold (fun m x -> m.Add( x, id )) fileNameToLanguage

let initialise (e: Browser.Types.HTMLDivElement) =

    let opts = typescript.typescriptDefaults.getCompilerOptions ()
    opts.jsx <- Some Typescript.JsxEmit.Preserve
    //opts.jsx <- Some Typescript.JsxEmit.ReactNative

    let options =
        {|   
            value = ""
            automaticLayout = true 
        |} |> As<Editor.IStandaloneEditorConstructionOptions>

    Monaco.editor.create (e, options) |> setEditor

let private getFileExtension( file : string) =
    match file.LastIndexOf('.') with
    | dot when dot >= 0 -> file.Substring(dot)
    | _ -> ""

let private createModelFromContent (name: string) content =
    let m =
        let lang = fileNameToLanguage.TryFind (getFileExtension name) |> Option.defaultValue "text"

        MonacoEditor.Monaco.editor.createModel (
            content,
            lang,
            MonacoEditor.Monaco.monaco.Uri.parse ("file:///" + name)
        )

    modelMap <- modelMap.Add(name, m)
    m

let getModel name content =
    if modelMap.ContainsKey name then
        let m = modelMap[ name ]
        m.setValue (Fable.Core.U2.Case1(content))
        m
    else
        createModelFromContent name content

let clear (editor: Monaco.Editor.IStandaloneCodeEditor) =
    editor.setModel (None: Monaco.Editor.ITextModel option)

let edit (editor:Monaco.Editor.IStandaloneCodeEditor) (name: string) (content : string)=
    editor.setModel (getModel name content |> Some)
