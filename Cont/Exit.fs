module Exit
open Cont

let private add1 x f = 1 + x |> f
let private mul5 x f = x * 5 |> f

let private exit a = make <| fun _ -> a
let private creturn a = callCC <| fun _ -> exit a

let private test2 x = 
    cont {
        let! r = make <| add1 x
        do! creturn 2
        let! r2 = make <| mul5 r
        return r2
    }

let rec private loop i f =
    cont {
        do! f i
        do! loop (i + 1) f
    }

let private test3 = 
    cont {
        do! callCC <|
            fun cbreak -> 
                loop 0 <| 
                        fun i ->
                            cont {
                                printfn ">>> i is:%O" i
                                if i > 2 then
                                    do! cbreak()
                                else return ()
                            }
    }

let exitTest() = 
    run (test2 3) id |> printfn "Result is:%O"
    run test3 id

