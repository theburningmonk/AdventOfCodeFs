#load "Day06.fs"

open System
open Day06

type Coordinate = int * int
type Action = TurnOn | TurnOff | Toggle

let parse (input : string) =
    let keywords = 
        [| "turn off"; "turn on"; "toggle"; "through" |]
    
    let [| start'; end' |] =
        input.Split(keywords, StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun x -> 
            x.Trim().Split(',')
            |> Array.map int
            |> function [| x; y |] -> x, y)

    let action =
        if input.StartsWith "turn on" then TurnOn
        elif input.StartsWith "turn of" then TurnOff
        else Toggle

    action, start', end'

let lights = Array2D.create 1000 1000 0

let perform (action, (startX, startY), (endX, endY)) =
    for x in startX..endX do
        for y in startY..endY do
            match action with
            | TurnOn  -> lights.[x, y] <- lights.[x, y] + 1
            | TurnOff -> lights.[x, y] <- max 0 (lights.[x, y] - 1)
            | Toggle  -> lights.[x, y] <- lights.[x, y] + 2

input.Split '\n'
|> Array.map parse
|> Array.iter perform

let mutable sum = 0

lights
|> Array2D.iter (fun n -> sum <- sum + n)