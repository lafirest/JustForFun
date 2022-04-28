
module Writer

type Writer<'s, 'a> = Writer of 's * 'a

let runWriter (Writer (_s, a)) = a 
let getWriter (Writer (s, _a)) = s

let add (a : int) (b : int) = a + b 

let tell a = Writer (a, ())

type WriterBuiler() =
    member this.Return(x) = Writer (0, x)
    member this.Bind((Writer (s, a)), f) = 
        let (Writer (s' : int, a')) = f a 
        Writer (s + s', a')
