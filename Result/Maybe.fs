module Maybe

type Maybe<'a> = Nothing | Just of 'a

type MaybeBuilder() =
    member this.Return(a) = Just a
    member this.Bind(m, f) =
        match m with 
            Nothing -> Nothing
            |Just a -> f a 
            
let maybe = new MaybeBuilder()
