// For more information see https://aka.ms/fsharp-console-apps
open Maybe
open Result

let mTest1 x y =
    match y with 
        0 -> Nothing
        |_ -> x / y |> Just

let mTest2 = 
    maybe {
        let! r1 = mTest1 10 2
        printfn "maybe monad, r1:%O" r1 
        let! r2 = mTest1 10 0
        printfn "maybe monad, r2:%O" r2
        return 0
    }

printfn "maybe monad, result:%O" mTest2

let rTest1 x y = 
    match y with 
        0 -> Error "y can't be 0"
        |_ -> x / y |> Result

let rTest2 x = 
    result {
        let! r = rTest1 10 x 
        return r * 2
    }

let rTest3 = 
    result {
        let! r1 = rTest2 2
        printfn "result monad, r1:%O" r1 
        let! r2 = rTest2 0
        printfn "result monad, r2:%O" r2
        return 0
    }

printfn "result monad, result:%O" rTest3


