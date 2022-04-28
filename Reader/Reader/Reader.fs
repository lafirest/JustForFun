module Reader

type Model<'r, 'a> = 'r -> 'a

type Reader<'r, 'a> = Reader of Model<'r, 'a>

let run (Reader m) r = m r
let ask = Reader id
let asks = Reader
let local f (Reader m) = f >> m |> Reader
let mkConst a b = a

type ReaderBuilder<'r, 'a>() =
        member this.Bind ((Reader m), f) = 
            Reader <| fun r -> run (f (m r)) r
        member this.Return(a) = mkConst a |> Reader 



