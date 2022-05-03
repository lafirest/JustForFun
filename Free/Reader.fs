module Reader
open Free

type Model<'r, 'a> = 'r -> 'a

type Reader<'r, 'a> = 
    Reader of Model<'r, 'a> interface IFunctor<'a> with
        member this.fmap f = 
            let (Reader F) = this 
            (Reader <| fun r -> F r |> f) :> IFunctor<'b>
    end

let ask<'r, 'a>  = Free <| Reader id 

let rec runReader F r = 
    match F with
        Pure a -> a
        |Free f ->
            let result = f.fmap (fun x -> runReader x r)
            let (Reader reader) = result :?> Reader<Free<obj>, 'a>
            reader r
