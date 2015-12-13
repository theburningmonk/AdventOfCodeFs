#load "Day01.fs"

open Day01

input
|> Seq.scan (fun n x -> 
    match x with 
    | '(' -> n+1 
    | _ -> n-1) 0
|> Seq.findIndex (fun n -> n = -1)