[<Measure>]
type Gold

[<Measure>]
type HP

type Character =
    {
        Name      : string
        HitPoints : int<HP>
        Damage    : int<HP>
        Armor     : int<HP>
    }

type Equipment =
    {
        Cost   : int<Gold>
        Damage : int<HP>
        Armor  : int<HP>
    }

type Shop =
    {
        Weapons : Equipment[]
        Armors  : Equipment[]
        Rings   : Equipment[]
    }

let you = 
    {
        Name      = "you"
        HitPoints = 100<HP>
        Damage    = 0<HP>
        Armor     = 0<HP>
    }

// set your input here
let boss =
    {
        Name      = "boss"
        HitPoints = 104<HP>
        Damage    = 8<HP>
        Armor     = 1<HP>
    }

let equipment cost damage armor =
    { Cost = cost; Damage = damage; Armor = armor }

let shop =
    {
        Weapons = 
            [|
                equipment 8<Gold>  4<HP> 0<HP>
                equipment 10<Gold> 5<HP> 0<HP>
                equipment 25<Gold> 6<HP> 0<HP>
                equipment 40<Gold> 7<HP> 0<HP>
                equipment 74<Gold> 8<HP> 0<HP>
            |]
        Armors =
            [|
                equipment 13<Gold>  0<HP> 1<HP>
                equipment 31<Gold>  0<HP> 2<HP>
                equipment 53<Gold>  0<HP> 3<HP>
                equipment 75<Gold>  0<HP> 4<HP>
                equipment 102<Gold> 0<HP> 5<HP>
            |]
        Rings =
            [|
                equipment 25<Gold>  1<HP> 0<HP>
                equipment 50<Gold>  2<HP> 0<HP>
                equipment 100<Gold> 3<HP> 0<HP>
                equipment 20<Gold>  0<HP> 1<HP>
                equipment 40<Gold>  0<HP> 2<HP>
                equipment 80<Gold>  0<HP> 3<HP>
            |]
    }

let weaponChoices = 
    shop.Weapons |> Seq.map (fun w -> [| w |])

let armorChoices = 
    seq {
        yield [||] // no armor
        for a in shop.Armors -> [| a |]
    }

let ringChoices =
    seq {
        yield [||] // no ring
        for r in shop.Rings -> [| r |] // 1 ring
        for i in 0..shop.Rings.Length-1 do
            for j in i+1..shop.Rings.Length-1 do
                // 2 rings
                yield [| shop.Rings.[i]; shop.Rings.[j] |]
    }

let equipmentCombos =
    seq {
        for w in weaponChoices do
            for a in armorChoices do
                for r in ringChoices do
                    yield Array.concat [| w; a; r |]
    }

let runSim boss you =
    let rec loop (attacker : Character) (defender : Character) =
        let damage = max 1<HP> (attacker.Damage - defender.Armor)
        let newHP  = defender.HitPoints - damage
        if newHP <= 0<HP>
        then attacker.Name
        else loop { defender with HitPoints = newHP } attacker

    loop you boss

let answer =
    equipmentCombos
    |> Seq.filter (fun equipments -> 
        { you with 
            Damage = equipments |> Array.sumBy (fun x -> x.Damage)
            Armor  = equipments |> Array.sumBy (fun x -> x.Armor) }
        |> runSim boss
        |> (=) "you")
    |> Seq.map (Array.sumBy (fun x -> x.Cost))
    |> Seq.min