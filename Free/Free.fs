module Free

type IFunctor<'a> = 
    abstract member fmap : ('a -> 'b) -> IFunctor<'b>

type Free<'a> =
    Pure of 'a
    | Free of IFunctor<(Free<'a>)> 

type FreeBuilder() =
    member this.Return(a) = Pure a
    member this.Bind(m, f) =
        match m with
            Pure a -> f a
            |Free g -> g.fmap (fun x -> this.Bind(x, f)) |> Free

let free = new FreeBuilder()
