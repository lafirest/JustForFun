// Learn more about F# at http://fsharp.org

open System
open Lens

type Skill = {
    Damage : int
    }

type Monster = {
    Name : string
    Level : int
    Skill : Skill
    }


[<EntryPoint>]
let main argv =
    let skillLens = mkLens (fun s -> s.Skill) (fun s a -> {s with Skill = a} )
    let damageLens = mkLens (fun s -> s.Damage) (fun s a -> {s with Damage = a} )
    let lens = damageLens >> skillLens

    let skill = {Damage = 100}
    let monster = {Name = "Monster"; Level = 14; Skill = skill} 

    view lens monster |> printfn "Get Damage is: %O" 
    over lens (fun _ -> 999 |> Identity :> IFunctor<int>) monster 
    |> printfn "Update Damage, New Monster is: %O" 
    0 // return an integer exit code
