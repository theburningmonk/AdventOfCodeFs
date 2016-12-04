#load "Day01.fs"

open System
open Day01

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