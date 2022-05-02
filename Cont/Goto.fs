module Goto
open Cont

//type fixedW = ('a -> Cont<'a, fixedW>)

let rec fix f = lazy f (fix f)

let private setJump =
    callCC <| fun w -> cont.Return(fix w)

let private goto (lb : Lazy<Cont<obj, obj>>) =
    cont{
        let! _ = lb.Value
        printfn "Here will never run"
        return ()  // just make return type correct
    }

let gotTest<'a,'b> target = 
    let mutable y = 0
    cont {
        let! x = setJump
        y <- y + 1
        printfn "y is:%O" y
        if y < target then
            do! goto x
        else return ()
    }
    
