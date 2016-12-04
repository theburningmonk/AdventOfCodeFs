#load "Day01.fs"

open System
open Day01

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