#load "Day24.fs"

open Day24

let weights = input.Split '\n' |> Seq.map int |> Seq.toList

let totalWeight = Seq.sum weights
let avgWeight   = totalWeight / 3

// generate possible groups that weight up to avg weight
let groups = 
    let rec loop (total, acc) rest =
        seq {
            match rest with
            | [] when total = avgWeight -> yield acc
            | [] -> ()

            | hd::rest ->
                if hd <= avgWeight - total then
                    yield! loop (total+hd, hd::acc) rest

                yield! loop (total, acc) rest
        }

    loop (0, []) weights |> Seq.toArray

let quantumEntanglement (gr : int list) = 
    gr |> Seq.map bigint |> Seq.reduce (*)

do groups 
   |> Array.sortInPlaceBy (fun gr ->
        gr.Length, quantumEntanglement gr)

let areMutExclusive [| g1; g2; g3 |] =
    let check g1 g2 = 
        g1 |> Seq.forall (fun n -> g2 |> Seq.forall ((<>) n))

    (check g1 g2) && (check g1 g3) && (check g2 g3)

{ 0..groups.Length-3 }
|> Seq.collect (fun idx ->
    { idx+1..groups.Length-2 } 
    |> Seq.collect (fun idx' -> 
        { idx+2..groups.Length-1 }
        |> Seq.map (fun idx'' -> 
            [| groups.[idx]; groups.[idx']; groups.[idx''] |])))
|> Seq.filter areMutExclusive
|> Seq.head
|> fun [| g1; _; _ |] -> quantumEntanglement g1