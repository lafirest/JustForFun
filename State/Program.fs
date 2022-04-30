// For more information see https://aka.ms/fsharp-console-apps
open State

let test1 x = 
    state {
        let! s = get
        do! put <| x + s
    }

let test2 x = 
    state {
        do! test1 5
        let! s = get
        return s + x
    }

exec (test2 3) 2 |> printfn "State is:%O"
eval (test2 3) 2 |> printfn "Result is:%O"
