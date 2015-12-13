#load "Day01.fs"

open Day01

input
|> Seq.fold (fun n x -> match x with | '(' -> n+1 | _ -> n-1) 0