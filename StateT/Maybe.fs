module Maybe
open IMonad

type Maybe<'a> = Nothing 
                | Just of 'a 
                    interface IFunctor<'a> 

type MaybeBuilder() =
    interface IMonadBuilder with
        member this.mreturn a = this.Return(a)
        member this.mbind m f = 
            let m2 = m :?> Maybe<'a>
            let f2 = fun x -> (f x) :?> Maybe<'b>
            this.Bind(m2, f2)
    member this.Return(a) = Just a
    member this.Bind(m, f) =
        match m with 
            Nothing -> Nothing
            |Just a -> f a 
            
let maybe = new MaybeBuilder()
