#load "Day14.fs"

open Day14

[<Measure>]
type s

[<Measure>]
type km

let inline ``km/s`` x =
    x |> int |> LanguagePrimitives.Int32WithMeasure<km/s>

let inline km x = 
    x |> int |> LanguagePrimitives.Int32WithMeasure<km>

let inline s x = 
    x |> int |> LanguagePrimitives.Int32WithMeasure<s>

type Status = Flying of int<s> | Resting of int<s>
type State  = 
    { 
        Name     : string
        Distance : int<km>
        FlyTime  : int<s>
        Speed    : int<km/s>
        RestTime : int<s>
        Status   : Status
        Score    : int
    }

let parse (line : string) =
    // e.g. Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
    let tokens = line.Split ' '
    {
        Name     = tokens.[0]
        Distance = 0<km>
        FlyTime  = s tokens.[6]
        Speed    = ``km/s`` tokens.[3]
        RestTime = s tokens.[13]
        Status   = Flying 0<s>
        Score    = 0
    }

let reindeer = input.Split '\n' |> Array.map parse

let step state = 
    match state.Status with
    | Flying n when n < state.FlyTime ->
        { state with 
            Status   = Flying (n + 1<s>)
            Distance = state.Distance + state.Speed * 1<s> }
    | Flying _ ->
        { state with Status = Resting 1<s>}

    | Resting n when n < state.RestTime ->
        { state with Status = Resting (n + 1<s>) }
    | Resting _ ->
        { state with
            Status   = Flying 1<s>
            Distance = state.Distance + state.Speed * 1<s> }

let award reindeer =
    let furthest = 
        reindeer 
        |> Array.map (fun x -> x.Distance)
        |> Array.max

    reindeer
    |> Array.map (fun x -> 
        if x.Distance = furthest 
        then { x with Score = x.Score + 1 }
        else x)

{ 1..2503 }
|> Seq.fold (fun reindeer _ ->
    reindeer |> Array.map step |> award) reindeer
|> Seq.maxBy (fun r -> r.Score)
|> fun r -> r.Score