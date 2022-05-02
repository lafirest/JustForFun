module Cont

type Model<'a, 'r> = ('a -> 'r) -> 'r
type Cont<'a, 'r> = Cont of Model<'a, 'r>

let run (Cont m) l = m l 

type ContBuiler() =
    member this.Return(a) = Cont <| fun n -> n a
    member this.Bind(m, f) =
        Cont <| fun out ->
            run m <| fun inc ->
                        run (f inc) out

let make = Cont
let cont = new ContBuiler()
let callCC f = make <| fun out -> 
        run (f <| fun a -> make <| fun _ -> out a) out

