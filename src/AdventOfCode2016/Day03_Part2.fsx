#load "Day03.fs"

open Day03

let numRows = input.Length

{ 0..numRows-1 } 
|> Seq.chunkBySize 3
|> Seq.collect (fun rows ->
  { 0..2 } 
  |> Seq.map (fun col -> 
    rows |> Array.map (fun row -> input.[row].[col])))
|> Seq.filter isTriangle
|> Seq.length