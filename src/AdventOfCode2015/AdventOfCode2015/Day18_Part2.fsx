#load "Day18.fs"

open Day18

let lights = Array2D.zeroCreate<bool> 100 100

let isCorner row col = 
    match (row, col) with
    | (0, 0) | (0, 99) | (99, 0) | (99, 99) -> true
    | _ -> false

// set init state for the lights
input.Split '\n'
|> Seq.iteri (fun row data ->
    data.Trim() 
    |> Seq.iteri (fun col char -> 
        if isCorner row col 
        then lights.[row, col] <- true
        else 
            match char with
            | '#' -> lights.[row, col] <- true
            | '.' -> lights.[row, col] <- false))

let countOnNeighbours row col (lights : bool[,]) =
    seq {
        for row' in row-1..row+1 do
            for col' in col-1..col+1 do
                if row' >=0 && row' < 100 &&
                   col' >=0 && col' < 100 &&
                   not (row' = row && col' = col)
                then yield lights.[row', col']
    }
    |> Seq.filter id
    |> Seq.length

let step lights =
    lights 
    |> Array2D.mapi (fun row col value ->
        if isCorner row col then true
        else
            match value with
            | true -> 
                match countOnNeighbours row col lights with
                | 2 | 3 -> true
                | _     -> false

            | false -> countOnNeighbours row col lights = 3)

let lights' = 
    { 1..100 } |> Seq.fold (fun lights' _ -> step lights') lights

let mutable count = 0
lights' |> Array2D.iter (function | true -> count <- count + 1 | _ -> ())