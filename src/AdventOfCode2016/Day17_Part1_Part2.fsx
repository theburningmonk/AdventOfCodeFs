#time

open System
open System.Text
open System.Security.Cryptography

let input = "udskfozm"
let width, height = 4, 4

let md5 = CryptoConfig.CreateFromName("MD5") :?> HashAlgorithm

let hash (input : string) =
  let bytes = input |> Encoding.UTF8.GetBytes |> md5.ComputeHash
  BitConverter.ToString(bytes).Replace("-", "").ToLower()

let inline between low hi x = x >= 0 && x < hi

let step direction (dx, dy) (x, y) path = 
  if x + dx |> between 0 width && y + dy |> between 0 height
  then Some((x+dx, y+dy), path + direction)
  else None
  
let up    = step "U" (0, -1)
let down  = step "D" (0, 1)
let left  = step "L" (-1, 0)
let right = step "R" (1, 0)

let findPaths input =
  [| ((0, 0), input) |]
  |> Seq.unfold (fun paths -> 
    paths 
    |> Array.filter (fun (pos, _) -> pos <> (3, 3))
    |> Array.collect (fun (pos, path) ->
      let hashVals = hash path |> Seq.take 4
      hashVals 
      |> Seq.zip [| up; down; left; right |]
      |> Seq.choose (fun (f, hashVal) ->
        if hashVal > 'a' then f pos path else None)
      |> Seq.toArray)
    |> function 
      | [||] -> None 
      | next -> Some(next, next))
  |> Seq.collect id
  |> Seq.choose (function
    | (3, 3), path -> Some <| path.Substring(input.Length)
    | _            -> None)

let part1 = findPaths input |> Seq.tryHead
let part2 = findPaths input |> Seq.map String.length |> Seq.max