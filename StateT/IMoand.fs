module IMonad

type IFunctor<'a> = interface end

type IMonadBuilder = 
    abstract mreturn : 'a -> IFunctor<'a>
    abstract mbind : IFunctor<'a> -> ('a -> IFunctor<'b>) -> IFunctor<'b>


