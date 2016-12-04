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

let move (x, y) n = function
  | N -> (x, y + n)
  | S -> (x, y - n)
  | E -> (x + n, y)
  | W -> (x - n, y)

input.Split(',')
|> Array.map (fun x -> x.Trim())
|> Array.fold (fun (dir, pos) (Instruction(angle, n)) ->
  let dir = turn (dir, angle)
  let x, y = move pos n dir
  (dir, (x, y))
) (N, (0, 0))
|> fun (_, (x, y)) -> Math.Abs x + Math.Abs y