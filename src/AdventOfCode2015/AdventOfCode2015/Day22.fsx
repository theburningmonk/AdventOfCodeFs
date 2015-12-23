[<Measure>]
type Mana

[<Measure>]
type HP

type Player =
    {
        HitPoints : int<HP>
        Armor     : int<HP>
        Mana      : int<Mana>
    }

type Boss =
    {
        mutable HitPoints : int<HP>
        Damage    : int<HP>
    }

type Effect = Player * Boss -> Player * Boss

type Game =
    {
        Player  : Player
        Boss    : Boss
        Effects : (Effect * int) list
    }

type Result = PlayerWin | BossWin

type Spell = Game -> Game

[<AutoOpen>]
module Spells =
    let private hitBoss damage (boss : Boss) =
        { boss with HitPoints = boss.HitPoints - damage }

    let private costMana cost player =
        { player with Mana = player.Mana - cost }

    let private heal hp (player : Player) =
        { player with HitPoints = player.HitPoints + hp }

    let missile game =
        { game with
            Boss   = hitBoss 4<HP> game.Boss
            Player = costMana 53<Mana> game.Player }

    let drain game =
        { game with
            Boss   = hitBoss 2<HP> game.Boss
            Player = costMana 73<Mana> game.Player 
                     |> heal 2<HP> }

    let private shieldEffect (player : Player, boss : Boss) =
        { player with Armor = player.Armor + 7<HP> }, boss

    let shield game =
        { game with
            Player  = costMana 113<Mana> game.Player 
            Effects = (shieldEffect, 6)::game.Effects }

    let private poisonEffect  (player : Player, boss : Boss) =
        player, hitBoss 3<HP> boss

    let poison game =
        { game with
            Player  = costMana 173<Mana> game.Player 
            Effects = (poisonEffect, 6)::game.Effects }

    let private rechargeEffect  (player : Player, boss : Boss) =
        { player with Mana = player.Mana + 101<Mana> }, boss

    let recharge game =
        { game with
            Player  = costMana 229<Mana> game.Player 
            Effects = (rechargeEffect, 5)::game.Effects }

    let spells = 
        [|
            missile,  53<Mana>
            drain,    73<Mana>
            shield,   113<Mana>
            poison,   173<Mana>
            recharge, 229<Mana>
        |]

let you =
    {
        HitPoints = 50<HP>
        Armor     = 0<HP>
        Mana      = 500<Mana>
    }

// use your input here
let boss =
    {
        HitPoints = 71<HP>
        Damage    = 10<HP>
    }

let hitPlayer damage (player : Player) =
    let damage = max 1<HP> (damage - player.Armor)
    { player with HitPoints = player.HitPoints - damage }

let applyEffects game =
    // first reset player's Armor to 0 before applying the
    // shield effects below
    let player = { game.Player with Armor = 0<HP> }
    let boss   = game.Boss

    // recursively apply the passive effects
    let player, boss =
        game.Effects
        |> List.fold (fun x (effect, _) -> effect x) (player, boss)

    let effects =
        game.Effects
        |> List.map (fun (effect, n) -> effect, n-1)
        |> List.filter (fun (_, n) -> n > 0)

    { game with 
        Player  = player
        Boss    = boss
        Effects = effects }

let (|IsGameOver|_|) { Player = player; Boss = boss } =
    if player.HitPoints <= 0<HP> then Some BossWin
    elif boss.HitPoints <= 0<HP> then Some PlayerWin
    else None

let runSim player boss =
    let rec playerTurn game totalMana = 
        match applyEffects game with
        | IsGameOver result -> 
            seq { yield result, totalMana }

        // not enough mana to cast any spell = instant lose
        | game when game.Player.Mana < 53<Mana> -> 
            seq { yield BossWin, totalMana }

        | game ->
            spells 
            |> Seq.filter (fun (_, cost) ->
                cost <= game.Player.Mana)
            |> Seq.collect (fun (castSpell, cost) ->
                let game      = castSpell game
                let totalMana = totalMana + cost

                if game.Boss.HitPoints <= 0<HP>
                then seq { yield PlayerWin, totalMana }
                else bossTurn game totalMana)

    and bossTurn game acc =
        match applyEffects game with
        | IsGameOver result -> seq { yield result, acc }

        | { Player = player; Boss = boss } as game -> 
            let player = hitPlayer boss.Damage player
            if player.HitPoints <= 0<HP>
            then seq { yield BossWin, acc }
            else playerTurn { game with Player = player } acc

    let game = { Player = player; Boss = boss; Effects = [] }
    playerTurn game 0<Mana>

runSim you boss
|> Seq.filter (fst >> (=) PlayerWin)
|> Seq.map snd
|> Seq.min