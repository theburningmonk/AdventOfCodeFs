#load "Day17.fs"

open Day17

let sizes = input.Split '\n' |> Seq.map int |> Seq.toList
let total = 150

let combine (sizes : int list) =
    let rec loop sizes (subtotal, acc) =
        seq {
            match sizes with
            | [] -> yield acc |> List.filter (snd >> (<>) 0)
            | size::rest ->
                let max = (total - subtotal) / size
                for n in 0..max do
                    let subtotal' = subtotal + n * size
                    yield! loop rest (subtotal', (size, n)::acc)
        }
    
    loop sizes (0, [])

combine sizes
|> Seq.filter (fun combo ->
    combo 
    |> List.sumBy (fun (size, n) -> size * n)
    |> (=) 150)
|> Seq.length