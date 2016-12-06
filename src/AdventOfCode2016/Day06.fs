module Day06

open System
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day06Input.txt")

let len = input.[0].Length

let solve sortBy =
  { 0..len-1 }
  |> Seq.map (fun idx -> input |> Array.map (fun line -> line.[idx]))
  |> Seq.map (fun col -> 
    col 
    |> Seq.groupBy id 
    |> Seq.map (fun (c, cs) -> c, Seq.length cs)
    |> sortBy snd
    |> Seq.head
    |> fst)
  |> fun chars -> new String(chars |> Seq.toArray)