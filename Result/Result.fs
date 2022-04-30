module Result

type Result<'r, 'e> = Result of 'r
                    | Error of 'e

type ResultBuilder() =
    member this.Return(a) = Result a
    member this.Bind(m, f) =
        match m with
            Result a -> f a
            | E -> E

let result = new ResultBuilder()
