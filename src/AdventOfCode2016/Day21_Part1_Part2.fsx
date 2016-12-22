open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day21Input.txt")

type Direction = Left | Right

type Instruction =
  | SwapPos              of int * int
  | SwapLetter           of char * char
  | Rotate               of Direction * int
  | RotateBasedOn        of char
  | Reverse              of int * int
  | Move                 of int * int

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let instructions = 
  input 
  |> Array.map (function
    | Regex "swap position (\d*) with position (\d*)" [ src; dest ] -> 
        SwapPos (int src, int dest)
    | Regex "swap letter (\w) with letter (\w)" [ x; y ] ->
        SwapLetter (x.[0], y.[0])
    | Regex "rotate right (\d*) step" [ n ] ->
        Rotate (Right, int n)
    | Regex "rotate left (\d*) step" [ n ] ->
        Rotate (Left, int n)
    | Regex "rotate based on position of letter (\w)" [ x ] ->
        RotateBasedOn x.[0]
    | Regex "reverse positions (\d*) through (\d*)" [ x; y ] ->
        Reverse (int x, int y)
    | Regex "move position (\d*) to position (\d*)" [ src; dest ] ->
        Move (int src, int dest)
  )

let swapPos src dest (input : char[]) =
   Array.init input.Length (fun idx -> 
    if idx = src then input.[dest] 
    elif idx = dest then input.[src]
    else input.[idx])

let swapLetter lf rt (input : char[]) =
  input |> Array.map (fun x -> 
    if x = lf then rt 
    elif x = rt then lf
    else x)

let rotate dir n (input : char[]) = 
  let n = n % input.Length

  let f = 
    if dir = Right 
    then fun idx -> (idx + input.Length - n) % input.Length
    else fun idx -> (idx + n) % input.Length

  Array.init input.Length (fun idx -> input.[f idx])

let rotateBasedOn char (input : char[]) =
  let idx = input |> Array.findIndex ((=) char)
  let n = 1 + idx + (if idx >= 4 then 1 else 0)
  rotate Right n input

let reverse startIdx endIdx (input : char[]) =
  Array.init input.Length (fun idx ->
    if idx < startIdx || idx > endIdx 
    then input.[idx]
    else input.[endIdx - (idx - startIdx)])

let move src dest (input : char[]) =
  let output = ResizeArray input
  let char = output.[src]
  output.RemoveAt(src)
  output.Insert(dest, char)
  output.ToArray()

let apply instructions input = 
  instructions
  |> Array.fold (fun input inst ->
    match inst with
    | SwapPos (src, dest) -> swapPos src dest input
    | SwapLetter (x, y)   -> swapLetter x y input
    | Rotate (dir, n)     -> rotate dir n input
    | RotateBasedOn (x)   -> rotateBasedOn x input
    | Reverse (x, y)      -> reverse x y input
    | Move (src, dest)    -> move src dest input) input
  |> fun chars -> new String(chars)

let part1 = "abcdefgh".ToCharArray() |> apply instructions

let rec insertions x = function
  | []             -> [[x]]
  | (y :: ys) as l -> (x::l)::(List.map (fun x -> y::x) (insertions x ys))

let rec permutations = function
  | []      -> seq [ [] ]
  | x :: xs -> Seq.concat (Seq.map (insertions x) (permutations xs))

let part2 = 
  permutations [ 'a'..'h' ]
  |> Seq.map List.toArray
  |> Seq.pick (fun pwd -> 
    match pwd |> apply instructions with
    | "fbgdceah" -> Some <| new String(pwd)
    | _ -> None)