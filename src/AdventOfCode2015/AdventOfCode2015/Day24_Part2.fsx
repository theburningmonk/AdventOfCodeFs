#load "Day24.fs"

open Day24

let weights = input.Split '\n' |> Seq.map uint64 |> Seq.toList

let totalWeight = Seq.sum weights
let avgWeight   = totalWeight / 4UL

let inline quantumEntanglement gr = gr |> Seq.reduce (*)

// since all weights are odd, and the avg weight (387) is odd
// we need odd number of packages
// smallest no. of packages we need is 5
let groups =
    let rec loop (total, acc) rest =
        seq {
            if total = avgWeight && List.length acc = 5 then 
                yield acc
            elif List.length acc < 5 then
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