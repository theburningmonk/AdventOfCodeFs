open System
open System.IO

let input = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/Day08Input.txt")

type Cmd =
  | Rect      of width:int * height:int
  | RotateRow of index:int * n:int
  | RotateCol of index:int * n:int

let (|StartsWith|_|) prefix (line : string) =
  if line.StartsWith(prefix) 
  then Some <| line.Substring(prefix.Length).Trim()
  else None

let (|SepBy|) (separator : string) (input : string) =
  input.Split([| separator |], StringSplitOptions.RemoveEmptyEntries) 
  |> Array.map int

let parse = function
  | StartsWith "rect" (SepBy "x" [| width; height |]) -> 
    Rect (width, height)
  | StartsWith "rotate row y=" (SepBy "by" [| y; n |]) ->
    RotateRow (y, n) 
  | StartsWith "rotate column x=" (SepBy "by" [| x; n |]) -> 
    RotateCol (x, n) 

let screen = Array2D.create 50 6 false

let rect width height =
  for y = 0 to height-1 do
    for x = 0 to width-1 do
      screen.[x, y] <- true
      
let rotateRow y n =
  let row = screen.[0.., y]
  for x in row.Length-1 .. -1 .. 0 do
    let x' = x - n
    if x' >= 0 
    then screen.[x, y] <- row.[x']
    else screen.[x, y] <- row.[row.Length + x']

let rotateCol x n =
  let col = screen.[x, 0..]
  for y in col.Length-1 .. -1 .. 0 do
    let y' = y - n
    if y' >= 0
    then screen.[x, y] <- col.[y']
    else screen.[x, y] <- col.[col.Length + y']

input
|> Array.map parse
|> Array.iter (function 
  | Rect (width, height) -> rect width height
  | RotateRow (y, n)     -> rotateRow y n
  | RotateCol (x, n)     -> rotateCol x n)

let part1 = 
  seq {
    for x = 0 to Array2D.length1 screen - 1 do
      for y = 0 to Array2D.length2 screen - 1 do
        yield screen.[x, y]
  }
  |> Seq.filter id
  |> Seq.length

// part 2
for y = 0 to Array2D.length2 screen - 1 do
  printfn ""
  for x = 0 to Array2D.length1 screen - 1 do
    match screen.[x, y] with
    | true -> printf "*"
    | _    -> printf " "