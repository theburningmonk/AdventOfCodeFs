#load "Day01.fs"

open System
open System.IO
open Day01

type Direction = N | S | E | W
type Turn = L | R

let (|Instruction|) (input: string) = 
  let dir = if input.[0] = 'L' then L else R
  let n   = input.Substring(1) |> int
  dir, n

let turn = function 
  | N, L | S, R -> W
  | S, L | N, R -> E
  | W, L | E, R -> S
  | E, L | W, R -> N

let move (x, y) n dir =
  { 1..n } 
  |> Seq.map (fun n ->
    match dir with
    | N -> (x, y + n)
    | S -> (x, y - n)
    | E -> (x + n, y)
    | W -> (x - n, y)
  )
  |> Seq.toArray

let follow instructions = 
  let rec loop pos dir instructions = seq {
    match instructions with
    | [] -> ()
    | (Instruction(angle, n))::rest -> 
      let dir = turn (dir, angle)
      let visited = move pos n dir
      yield! visited
      
      let lastPos = visited |> Array.last
      yield! loop lastPos dir rest
  }

  loop (0, 0) N instructions

input.Split(',') 
|> Seq.map (fun x -> x.Trim()) 
|> Seq.toList
|> follow
|> Seq.groupBy id
|> Seq.filter (fun (_, ps) -> Seq.length ps > 1)
|> Seq.head
|> fun ((x, y), _) -> Math.Abs x + Math.Abs y