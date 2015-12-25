#load "Day19.fs"

open Day19
open System

let string (chars : char seq) = new String(Seq.toArray chars)

let lines =
    input.Split '\n'
    |> Array.filter (not << String.IsNullOrWhiteSpace)

let tokens =
    lines
    |> Seq.takeWhile (fun s -> s.Contains "=>")
    |> Seq.map (fun s -> 
        s.Split ' ' |> Seq.head |> Seq.rev |> string)
    |> Set.ofSeq

let molecule = 
    (lines |> Seq.last)
        .Replace("Rn", "(")
        .Replace("Ar", ")")
        .Replace("Y", ",")
    |> Seq.rev
    |> string

let count (molecule : string) =
    let rec loop (t, p, c) = function
        | [] -> t, p, c
        | '('::tl 
        | ')'::tl -> 
            loop (t+1, p+1, c) tl
        | ','::tl ->
            loop (t+1, p, c+1) tl
        | hd::tl when tokens.Contains (string [hd]) ->
            loop (t+1, p, c) tl
        | hd1::hd2::tl when tokens.Contains (string [hd1; hd2]) ->
            loop (t+1, p, c) tl

    loop (0, 0, 0) (molecule |> Seq.toList)

let t, p, c = count molecule
let answer = t - p - 2*c - 1