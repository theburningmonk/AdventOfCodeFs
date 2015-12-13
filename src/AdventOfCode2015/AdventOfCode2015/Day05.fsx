#load "Day05.fs"

open Day05

let (<&&>) f g x = f x && g x

let atLeast3Vowels (word : string) =
    word
    |> Seq.filter (function 
        | 'a' | 'e' | 'i' | 'o' | 'u' -> true 
        | _ -> false)
    |> Seq.length
    |> (fun n -> n >= 3)

let atLeast1Double (word : string) =
    word
    |> Seq.windowed 2
    |> Seq.exists (fun [| a; b |] -> a = b)

let noThese lst (word : string) =
    lst 
    |> List.exists word.Contains
    |> not

input.Split('\n')
|> Array.map (fun x -> x.Trim())
|> Array.filter 
    (atLeast3Vowels <&&> 
     atLeast1Double <&&> 
     noThese [ "ab"; "cd"; "pq"; "xy" ])
|> Array.length