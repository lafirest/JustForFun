module Lens

type IFunctor<'a> = 
    abstract fmap : ('a -> 'b) -> IFunctor<'b>

type Const<'a, 'b> = 
    Const of 'a interface IFunctor<'b> with
                    member this.fmap _ =
                        let (Const c) = this in Const c :> IFunctor<'c>

type Identity<'a> = 
    Identity of 'a interface IFunctor<'a> with
                    member this.fmap (f : 'a -> 'b) = 
                        let (Identity id) = this in
                           f id |> Identity :> IFunctor<'b>

let fmap<'a, 'b> f (F : IFunctor<'a>) = F.fmap f : IFunctor<'b>

let inline mkLens (getter : 's -> 'a) (setter : 's -> 'b -> 'c) =
   fun (f : 'a -> IFunctor<'b>) (s : 's) -> fmap (setter s) (f (getter s)) : IFunctor<'c> 

let view<'a, 's> 
    (lens : ('a -> IFunctor<'a>) -> 's -> IFunctor<'s>) 
    (s : 's) = 
    let toFunctor = fun x -> Const x :> IFunctor<'a>
    let (Const c) = lens toFunctor s :?> Const<'a, 's> in 
        c

let over<'a, 's> 
    (lens : ('a -> IFunctor<'a>) -> 's -> IFunctor<'s>) 
    f 
    s = 
    let toFunctor = fun x -> Identity x :> IFunctor<'a>
    let (Identity r) =  (lens (toFunctor >> f) s) :?> Identity<'s> in
        r
