// For more information see https://aka.ms/fsharp-console-apps
open Free
open Reader

let v1 = free {
        let! r = ask
        printfn "r is:%O" r
        return 3
    }

let v2 = free {
        let! v1r = v1
        let! r = ask
        let rint = r :?> int
        return rint + v1r
    }

runReader v2 (Pure 10) |> printfn "result is:%O"
