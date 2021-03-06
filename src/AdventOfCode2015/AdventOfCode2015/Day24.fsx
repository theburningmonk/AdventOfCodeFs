﻿#load "Day24.fs"

open Day24

let weights = input.Split '\n' |> Seq.map uint64 |> Seq.toList

let totalWeight = Seq.sum weights
let avgWeight   = totalWeight / 3UL

let inline quantumEntanglement gr = gr |> Seq.reduce (*)

// since all weights are odd, and the avg weight (516) is even
// we need even number of packages
// smallest no. of packages we need is 6
let groups =
    let rec loop (total, acc) rest =
        seq {
            if total = avgWeight && List.length acc = 6 then 
                yield acc
            elif List.length acc < 6 then
                match rest with
                | [] -> ()
                | hd::rest -> 
                    yield! loop (hd+total, hd::acc) rest
                    yield! loop (total, acc) rest
        }

    loop (0UL, []) weights

groups
|> Seq.map quantumEntanglement
|> Seq.min