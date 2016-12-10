open System
open System.IO

let input = File.ReadAllText(__SOURCE_DIRECTORY__ + "/Day09Input.txt")

let (|Int|_|) x = 
  match Int32.TryParse x with
  | true, x -> Some x
  | _       -> None

let (|Repeat|_|) (input : string) = 
  match input.Split('x') with
  | [| Int n; Int count |] -> Some (n, count)
  | _                      -> None

let decompressV1 input =
  let rec loop (input : string) acc =
    match input.Split([| '('; ')' |], 3) with
    | [| text |] -> acc + text.Length
    | [| head; Repeat(n, count); rest |] ->
      let acc = acc + head.Length + n * count
      loop (rest.Substring n) acc
    
  loop input 0

let part1 = decompressV1 input

let rec decompressV2 input =
  let rec loop (input : string) acc =
    match input.Split([| '('; ')' |], 3) with
    | [| text |] -> acc + int64 text.Length
    | [| head; Repeat(n, count); rest |] ->
      let repeatLen = rest.Substring(0, n) |> decompressV2
      let acc = acc + int64 head.Length + int64 count * repeatLen
      loop (rest.Substring n) acc
    
  loop input 0L

let part2 = decompressV2 input