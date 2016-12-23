#time

open System
open System.Collections.Generic
open System.IO

let inputs = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/Day23Input.txt")

type Operant = 
  | Value of int
  | Reg   of string

type Instruction =
  | Cpy of n: Operant * dest: Operant
  | Inc of string
  | Dec of string
  | Jnz of n: Operant * offset: Operant
  | Tgl of reg: string

let (|Operant|) x = 
  match Int32.TryParse x with
  | true, n -> Value n
  | _       -> Reg x

let parse (line : string) =
  match line.Split() with
  | [| "cpy"; Operant n; Operant dest |]   -> Cpy (n, dest)
  | [| "inc"; reg |]                       -> Inc reg
  | [| "dec"; reg |]                       -> Dec reg
  | [| "jnz"; Operant n; Operant offset |] -> Jnz (n, offset)
  | [| "tgl"; reg |]                       -> Tgl reg

let toggle = function
  | Inc reg -> Dec reg
  | Dec reg | Tgl reg -> Inc reg
  | Cpy (n, dest)     -> Jnz (n, dest)
  | Jnz (n, offset)   -> Cpy (n, offset)

let execute initValues (inputs : string[]) =
  let registers = new Dictionary<string, int>()
  initValues |> Seq.iter (fun (key, value) -> registers.[key] <- value)

  let instructions = inputs |> Array.map parse

  let fetch = function
    | Value n -> n
    | Reg reg -> 
      match registers.TryGetValue reg with
      | true, n -> n
      | _       -> 0

  let rec loop idx = 
    if idx >= instructions.Length then registers
    else 
      match instructions.[idx] with
      | Cpy (n, Reg dest) -> 
        registers.[dest] <- fetch n
        loop (idx+1)
      | Inc reg ->
        registers.[reg] <- registers.[reg] + 1
        loop (idx+1)
      | Dec reg ->
        registers.[reg] <- registers.[reg] - 1
        loop (idx+1)
      | Jnz (n, offset) when fetch n <> 0 ->
        loop (idx+fetch offset)
      | Tgl reg ->
        let offset = registers.[reg]
        if idx+offset < instructions.Length then
          instructions.[idx+offset] <- toggle instructions.[idx+offset]

        loop (idx+1)
      | _ -> loop (idx+1)

  loop 0

let part1 = (execute [ "a", 7 ] inputs).["a"]
let part2 = (execute [ "a", 12 ] inputs).["a"]