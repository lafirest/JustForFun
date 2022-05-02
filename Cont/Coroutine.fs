module Coroutine
open Cont
open System.Collections.Generic

type Task = Cont<unit, unit>

let private queue = new Queue<Task>()

let private cyield = 
    cont {
        do! callCC <| fun w -> 
            let next = w() 
            queue.Enqueue(next)
            if queue.Count <> 0 then
                run (queue.Dequeue()) id
            make <| fun _ -> ()
    }
    
let private put task = queue.Enqueue(task)
let private start() = run (queue.Dequeue()) ignore 

let coroutineTest() =
    let mkTask x = cont {
        printfn ">>> Task:%O Pos:1" x
        do! cyield
        printfn ">>> Task:%O Pos:2" x
        do! cyield
        }

    put <| mkTask 1
    put <| mkTask 2
    put <| mkTask 3
    start()


