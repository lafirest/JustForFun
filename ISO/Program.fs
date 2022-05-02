open Lens

type Skill = {
    Damage : int
    }

type Monster = {
    Name : string
    Skill : Skill
    }

let fwm m = (m.Skill, fun s -> {m with Skill = s})
let fws s = (s.Damage, fun d -> {s with Damage = d})
let bw (l, r) = r l 

let com f1 f2 s =
    let (s2, r1) = f1 s
    let (s3, r2) = f2 s2
    (s3, r2 >> r1)

let lens = Lens (com fwm fws, bw)

let monster = {Name = "A"; Skill = {Damage = 12}}

get lens monster |> printfn "Level is:%O"
put lens 33 monster |> printfn "New Monster is:%O"
