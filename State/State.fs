module State

type Model<'s, 'a> = 's -> 's * 'a
type State<'s, 'a> = State of Model<'s, 'a>

let run (State m) s = m s
let eval m s = let (_, a) = run m s in a
let exec m s = let (s, _) = run m s in s

let get<'s,'a> : State<'s, 's> = State <| fun s -> (s, s)
let put s = State <| fun _ -> (s, ())

type StateBuilder() =
    member this.Return(a) = State <| fun s -> (s, a)
    member this.Bind(m, f) = 
        State <| fun s ->
                    let (s', a') = run m s 
                    run (f a') s'


let state<'s, 'a> = new StateBuilder()

