module Log 
open Sutil

let mutable idCounter = 0
let nextId() =
    let id = idCounter + 1
    idCounter <- id
    id

type LogMessage = 
    {
        Id : int
        Text : string
    }
    static Create(s: string) = { Id = nextId(); Text = s }

let messages = Store.make ([] : LogMessage list)
let log (s:string) =
    messages |> Store.modify (fun ms -> ms @ [ LogMessage.Create s ])

let clear() =
    messages |> Store.modify (fun _ -> [])

let viewMessage (m : LogMessage) =
//        Html.div [
        Html.pre [ text m.Text ]
//        ]
