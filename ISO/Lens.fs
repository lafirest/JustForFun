module Lens

type ISO<'a,'b> = ('a -> 'b) * ('b -> 'a)
type Lens<'a, 'b, 'r> = Lens of ISO<'a, 'b * 'r>

let first f (a, b) =
    (f a, b)

let private mkConst a b = a

let get (Lens iso) = fst iso >> fst
let modify (Lens iso) f = fst iso >> first f >> snd iso
let put l a = modify l (mkConst a)

