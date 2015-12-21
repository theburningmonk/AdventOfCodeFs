#load "Day19.fs"

open Day19
open System
open System.Text.RegularExpressions

type InputKind =
    | Replacement  of string * string
    | Molecule     of string

let split (by : string) (x : string) =
    x.Split(
        [| by |],
        StringSplitOptions.RemoveEmptyEntries)

let parse (input : string) =
    match split " => " <| input.Trim() with
    | [| from; to' |] -> Replacement (from, to')
    | _ -> Molecule input 

let replacements, [| Molecule(medicine) |] = 
    split "\n" input 
    |> Array.filter (not << String.IsNullOrWhiteSpace)
    |> Array.map parse
    |> Array.partition (function
        | Replacement _ -> true 
        | _ -> false)

// work backwards compared to Part 1, i.e. given current 
// string, which replacements could have given this
let revert molecule =
    replacements
    |> Seq.collect (fun (Replacement (from, to')) ->
        let regex = new Regex(to')
        regex.Matches molecule
        |> Seq.cast<Match>
        |> Seq.map (fun m ->
            molecule.[0..m.Index-1] + from + molecule.[m.Index+m.Length..]))

let answer =
    let state = seq { yield 0, medicine }

    { 0..100 }
    |> Seq.scan (fun molecules _ ->
        molecules 
        |> Seq.collect (fun (n, molecule) ->
            revert molecule
            |> Seq.map (fun m -> n+1, m))) state
    |> Seq.collect id
    |> Seq.filter (fun (_, molecule) -> molecule = "e")
    |> Seq.head
    |> fst