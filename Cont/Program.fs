open Cont
open Exit
open Coroutine
open Goto

let add1 x f = 1 + x |> f
let mul5 x f = x * 5 |> f

let test x = 
    cont {
        let! r = make <| add1 x
        let! r2 = make <| mul5 r
        return r2
    }

run (test 3) id |> printfn "Result is:%O"

exitTest()
coroutineTest()
run (gotTest 5) (fun _ -> ()) |> ignore

