#load "Day17.fs"

open Day17

let sizes = input.Split '\n' |> Seq.map int |> Seq.toList
let total = 150

let combine (sizes : int list) =
    let rec loop sizes (subtotal, acc) =
        seq {
            match sizes with
            | [] when subtotal = total -> yield acc
            | [] -> ()
            | size::tl ->
                yield! loop tl (subtotal, acc)
                if subtotal + size <= total then
                    yield! loop tl (subtotal+size, size::acc)
        }
    
    loop sizes (0, [])

let combos = combine sizes |> Seq.toArray
let minLen = combos |> Array.map List.length |> Array.min
combos
|> Seq.filter (List.length >> (=) minLen)
|> Seq.length