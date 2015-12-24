#load "Day23.fs"

open System
open Day23

type Instruction =
    | Hlf of string
    | Tpl of string
    | Inc of string
    | Jmp of int
    | Jie of string * int
    | Jio of string * int

let instructions =
    input.Split '\n'
    |> Array.map (fun line ->
        line.Split([|' '; ','|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun s -> s.Trim())
        |> function 
            | [| "hlf"; r |] -> Hlf r
            | [| "tpl"; r |] -> Tpl r 
            | [| "inc"; r |] -> Inc r 
            | [| "jmp"; offset |]    -> Jmp <| int offset
            | [| "jie"; r; offset |] -> Jie (r, int offset)
            | [| "jio"; r; offset |] -> Jio (r, int offset))

let carryOut (instructions : Instruction[]) =
    let rec loop idx (regs : Map<string, int>) = 
        if idx >= instructions.Length then regs
        else
            match instructions.[idx] with
            | Hlf r -> loop (idx+1) <| regs.Add(r, regs.[r]/2)
            | Tpl r -> loop (idx+1) <| regs.Add(r, regs.[r]*3)
            | Inc r -> loop (idx+1) <| regs.Add(r, regs.[r]+1)
            | Jmp offset -> loop (idx+offset) regs
            | Jie (r, offset) ->
                if regs.[r] % 2 = 0 
                then loop (idx+offset) regs
                else loop (idx+1) regs
            | Jio (r, offset) ->
                if regs.[r] = 1
                then loop (idx+offset) regs
                else loop (idx+1) regs

    loop 0 <| Map.ofArray [| ("a", 1); ("b", 0) |]

(carryOut instructions).["b"]