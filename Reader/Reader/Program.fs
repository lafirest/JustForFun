open Reader

let reader = new ReaderBuilder<int, int>()

let add x = 
    reader {
        let! r = ask
        return r + x
    }

let sub x = 
    reader {
        let! r = ask
        return r - x
    }

let test1 = 
    reader {
        let! r = ask
        let! x = add 1
        let! y = sub 2
        return (r, x * y)
    }

let test2 = 
    reader {
        let! x = local (fun r -> r * 2) test1
        return x
    }

[<EntryPoint>]
let main argv =
    run test1 5 |> printfn ">>> resut:%O"
    run test2 5 |> printfn ">>> resut:%O"
    0

