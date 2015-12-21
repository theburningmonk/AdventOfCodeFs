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

replacements
|> Seq.collect (fun (Replacement (from, to')) -> 
    let regex = new Regex(from)
    regex.Matches(medicine)
    |> Seq.cast<Match>
    |> Seq.map (fun m ->
        medicine.[0..m.Index-1] + to' + medicine.[m.Index+m.Length..]))
|> Seq.distinct
|> Seq.length