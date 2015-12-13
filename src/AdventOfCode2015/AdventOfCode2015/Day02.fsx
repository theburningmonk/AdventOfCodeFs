#load "Day02.fs"

open System
open Day02

let split (by : char) (x : string) = 
    x.Split([| by |], StringSplitOptions.RemoveEmptyEntries)

input
|> split '\n'
|> Array.map (fun l -> l |> split 'x' |> Array.map int)
|> Array.sumBy (fun [| l; w; h |] ->
    let area  = 2*l*w + 2*w*h + 2*h*l
    let extra = [ l*w; w*h; l*h ] |> List.min
    area + extra)