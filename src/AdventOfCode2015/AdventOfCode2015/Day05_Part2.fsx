#load "Day05.fs"

open Day05

let (<&&>) f g x = f x && g x

let atLeastPairAppearTwice (word : string) =
    seq { 0..word.Length-2 }
    |> Seq.exists (fun i -> 
        let pair = word.Substring(i, 2)
        let rest = word.Remove(i, 2)
        rest.Contains pair)

let atLeastOneRepeatedLetter (word : string) =
    seq { 0..word.Length-3}
    |> Seq.exists (fun i -> word.[i] = word.[i+2])

input.Split('\n')
|> Array.filter 
    (atLeastPairAppearTwice <&&> 
     atLeastOneRepeatedLetter)
|> Array.length