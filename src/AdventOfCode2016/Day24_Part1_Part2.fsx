#time

open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day24Input.txt")

type Tile = Wall | Space | Number of int

let width, height = input.[0].Length, input.Length
let grid = Array2D.init width height (fun x y -> 
  match input.[y].[x] with
  | '#' -> Wall
  | '.' -> Space
  | num -> num |> string |> int |> Number)

let numbers = 
  [|
    for y = 0 to height-1 do
      for x = 0 to width-1 do
        match grid.[x, y] with
        | Number n -> yield n, (x, y)
        | _ -> ()
  |]
  |> Map.ofArray

open System.Collections.Generic

let findShortestPath start target =
  let cache = new HashSet<int*int>([start])

  [| (start, 0) |]
  |> Seq.unfold (fun paths ->
    paths
    |> Seq.collect (fun ((x, y), moves) -> 
      [ x-1, y; x+1, y; x, y-1; x, y+1 ]
      |> Seq.filter (fun (x', y') ->
        grid.[x', y'] <> Wall && cache.Add(x', y'))
      |> Seq.map (fun (x', y') -> (x', y'), moves+1))
    |> Seq.toArray
    |> function
      | [||] -> None
      | newStates -> Some(newStates, newStates)
  )
  |> Seq.collect id
  |> Seq.pick (fun (pos, moves) -> if pos = target then Some moves else None)

let pairDistances =
  [|
    for src = 0 to 6 do
      for dest = src + 1 to 7 do
        let distance = findShortestPath numbers.[src] numbers.[dest]
        yield (src, dest), distance
        yield (dest, src), distance
  |]
  |> Map.ofArray

let part1 = 
  let rec traverse current (nodes : Set<int>) acc =
    if nodes.Count = 0 then acc
    else
      nodes
      |> Seq.map (fun next -> 
        let left = nodes.Remove next
        let dist = pairDistances.[current, next]

        traverse next left (acc + dist))
      |> Seq.min

  traverse 0 (set [1..7]) 0

let part2 = 
  let rec traverse current (nodes : Set<int>) finalDest acc =
    if nodes.Count = 0 then acc + pairDistances.[current, finalDest]
    else
      nodes
      |> Seq.map (fun next -> 
        let left = nodes.Remove next
        let dist = pairDistances.[current, next]

        traverse next left finalDest (acc + dist))
      |> Seq.min
      
  traverse 0 (set [1..7]) 0 0