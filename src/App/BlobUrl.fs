module BlobUrl

open Fable.Core.JsInterop
open Fable.Core
open Fable.ZipJs

open SutilOxide.FileSystem
open Browser.Types
open Browser.Blob

type MimeType =
    | Html
    | JavaScript
    | Css
    | Text

let [<Global>] URL: obj = jsNative

let zip : obj = importAll "@zip.js/zip.js/lib/zip-no-worker.js"

Zip.configure (jsOptions<IConfiguration> (fun x -> x.useWebWorkers <- false))

let createZip (fs: IFileSystem) = promise {
    let writer = Zip.createBlobWriter()
    let zipWriter = Zip.createZipWriter(writer)

    let rec zipFolder folder =
        promise {
            for d in fs.Folders(folder) do
                do! zipFolder (IFileSystem.Combine(folder,d))

            for f in fs.Files(folder) do
                let path = IFileSystem.Combine(folder, f)
                let content = fs.GetFileContent(path)
                let! e = zipWriter.add(path, Zip.createStringReader content)
                return ()
        }

    do! zipFolder("/")
    do! zipWriter.close()

    return writer.getData()
}

type File = {
    Name : string
    Content : string
}

let createZipFromFileArray (files: File[]) = promise {
    let writer = Zip.createBlobWriter()
    let zipWriter = Zip.createZipWriter(writer)

    let zipFolder folder =
        promise {
            for f in files do
                let path = IFileSystem.Combine(folder, f.Name)
                let content = f.Content
                let! e = zipWriter.add(path, Zip.createStringReader content)
                return ()
        }

    do! zipFolder("/")
    do! zipWriter.close()

    return writer.getData()
}

let existsZip ( data: Blob, test : (string -> bool) ) =
    let breader = Zip.createBlobReader(!!data)
    let zreader = Zip.createZipReader( breader )
    promise {
        let! entries = zreader.getEntries()
        return (entries |> Array.exists (fun e -> 
//            Log.Log.Trace.log(e.filename)
            test e.filename
        ))
    }

let extractZip ( data: Blob, fs : IFileSystem, folder : string ) =
    let breader = Zip.createBlobReader(!!data)
    let zreader = Zip.createZipReader( breader )
    promise {
        let! entries = zreader.getEntries()

        for entry in entries do
            let! text = entry.getData( Zip.createStringWriter() )
            let target = IFileSystem.Combine(folder, entry.filename)
            // Log.Trace.log("Extracting " + target)
            fs.SetFileContent(  target, !!text )
            ()
        return ()
    }

let generateBlobURL content mimeType : string =
    let parts: obj[] = [| content |]
    let options =
        jsOptions<BlobPropertyBag>(fun o ->
            o.``type`` <-
                match mimeType with
                | Html -> "text/html"
                | JavaScript -> "text/javascript"
                | Css -> "text/css"
                | Text -> "text/plain")
    URL?createObjectURL(Blob.Create(parts, options))

let downloadZip (z:Blob) (name: string) = 
    let a = Browser.Dom.document.createElement("a") :?> Browser.Types.HTMLAnchorElement
    a?download <- if name.EndsWith(".zip") then name else (name + ".zip")
    a.href <- generateBlobURL z Text
    a.onclick <- fun e -> Browser.Dom.document.body.removeChild(e.target :?> Browser.Types.Node) |> ignore
    Browser.Dom.document.body.appendChild(a) |> ignore
    a.click()

let downloadFs (fs:IFileSystem) (name: string) = promise {
    let! z = createZip fs
    downloadZip z name
    return()
}

let downloadFileArray (files:File[]) (name: string) = promise {
    let! z = createZipFromFileArray files
    downloadZip z name
    return()
}
