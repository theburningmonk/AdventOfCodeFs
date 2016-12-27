#time

open System
open System.Collections.Generic
open System.IO

let inputs = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/Day25Input.txt")

type Operant = 
  | Value of int
  | Reg   of string

type Instruction =
  | Cpy of n: Operant * dest: Operant
  | Inc of string
  | Dec of string
  | Jnz of n: Operant * offset: Operant
  | Out of n: Operant

let (|Operant|) x = 
  match Int32.TryParse x with
  | true, n -> Value n
  | _       -> Reg x

let instructions = 
  inputs 
  |> Array.map (fun l -> l.Split()) 
  |> Array.map (function
    | [| "cpy"; Operant n; Operant dest |]   -> Cpy (n, dest)
    | [| "inc"; reg |]                       -> Inc reg
    | [| "dec"; reg |]                       -> Dec reg
    | [| "jnz"; Operant n; Operant offset |] -> Jnz (n, offset)
    | [| "out"; Operant n |]                 -> Out n)

let execute initValues (instructions : Instruction[]) =
  let registers = new Dictionary<string, int>()
  initValues |> Seq.iter (fun (key, value) -> registers.[key] <- value)

  let fetch = function
    | Value n -> n
    | Reg reg -> 
      match registers.TryGetValue reg with
      | true, n -> n
      | _       -> 0

  let rec loop idx = seq {
    if idx >= instructions.Length then ()
    else 
      match instructions.[idx] with
      | Cpy (n, Reg dest) -> 
        registers.[dest] <- fetch n
        yield! loop (idx+1)
      | Inc reg ->
        registers.[reg] <- registers.[reg] + 1
        yield! loop (idx+1)
      | Dec reg ->
        registers.[reg] <- registers.[reg] - 1
        yield! loop (idx+1)
      | Jnz (n, offset) when fetch n <> 0 ->
        yield! loop (idx+fetch offset)
      | Out n ->
        yield fetch n
        yield! loop (idx+1)
      | _ -> 
        yield! loop (idx+1)
  }
  
  loop 0

let part1 = 
  Seq.initInfinite id
  |> Seq.find (fun n -> 
    execute [ "a", n ] instructions 
    |> Seq.take 1000
    |> Seq.chunkBySize 2
    |> Seq.forall (fun [| a; b |] -> a = 0 && b = 1))

let _ = Inc ""