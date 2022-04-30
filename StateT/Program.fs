open StateT
open Maybe

let test1 x y = 
    match y with
        0 -> Nothing
        |_ -> x / y |> Just

let test2 y =
    smaybe {
        let! r1 = test1 10 2 |> liftM
        printfn "r1 is:%O" r1
        let! s = get
        do! put (r1 * s)
        let! r2 = test1 s y |> liftM
        printfn "r2 is:%O" r2
        return r2
    }

runST (test2 1) 2 |> printfn "Raw result with 1 is: %O"
runST (test2 0) 2 |> printfn "Raw result with 0 is: %O"

