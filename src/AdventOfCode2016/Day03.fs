module Day03

open System
open System.IO

let input = 
  File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day03Input.txt")
  |> Array.map (fun line -> 
    line.Split() 
    |> Array.filter (not << String.IsNullOrWhiteSpace)
    |> Array.map int)

let isTriangle [| a; b; c |] = a + b > c && b + c > a && a + c > b