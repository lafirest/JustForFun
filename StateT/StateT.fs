module StateT
open IMonad
open Maybe
    
type Model<'s, 'a> = 's -> IFunctor<'s * 'a>
type StateT<'s, 'a> = StateT of Model<'s, 'a>
let runST (StateT m) s = m s

type StateTBuiler(im : IMonadBuilder) =
    member this.IM = im
    member this.Return(a)= StateT <| fun s -> im.mreturn (s, a)
    member this.Bind(m, f) =
        StateT <| fun s ->
            let imr = runST m s
            im.mbind imr (fun (s2, a2) -> runST (f a2) s2)

let smaybe = new StateTBuiler(new MaybeBuilder() :> IMonadBuilder)
let get<'s> = StateT <| fun (s : 's) -> smaybe.IM.mreturn (s, s)
let put s = StateT <| fun _ -> smaybe.IM.mreturn (s, ())
let liftM m = StateT <| fun s -> 
                            match m with
                                Nothing -> Nothing
                                | Just a -> Just (s, a)


