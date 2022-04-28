// Learn more about F# at http://fsharp.org

open System
open Writer

let writer = new WriterBuiler()

let test1 : Writer<int, int> =
    writer {
        do! tell 7
        do! tell 3
        return 3
    }

let test2 : Writer<int, bool> =
    writer {
        let! x = test1
        do! x + 3 |> tell
        return true 
    }

[<EntryPoint>]
let main argv =
    runWriter test2 |> printfn ">>> result:%O"
    getWriter test2 |> printfn ">>> result:%O"
    0 
