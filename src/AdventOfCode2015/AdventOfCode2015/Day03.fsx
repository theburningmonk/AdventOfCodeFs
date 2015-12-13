#load "Day03.fs"

open Day03

input
|> Seq.fold (fun ((x, y)::_ as acc) nxt ->
    match nxt with
    | '^' -> (x, y-1)::acc
    | '>' -> (x+1, y)::acc
    | 'v' -> (x, y+1)::acc
    | '<' -> (x-1, y)::acc) [(0,0)]
|> Seq.distinct
|> Seq.length