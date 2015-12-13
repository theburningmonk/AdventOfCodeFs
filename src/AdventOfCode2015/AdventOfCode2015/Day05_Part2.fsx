#load "Day05.fs"

open Day05

let (<&&>) f g x = f x && g x

let atLeastPairAppearTwice (word : string) =
    seq { 0..word.Length-2 }
    |> Seq.exists (fun i -> 
        let pair = [| word.[i]; word.[i+1] |]
        [|
            yield! word.[0..i-1]
            yield! [| '-'; '-' |]
            yield! word.[i+2..]
        |]
        |> Seq.windowed 2
        |> Seq.exists ((=) pair))

let atLeastOneRepeatedLetter (word : string) =
    seq { 0..word.Length-3}
    |> Seq.exists (fun i -> word.[i] = word.[i+2])

input.Split('\n')
|> Array.filter 
    (atLeastPairAppearTwice <&&> 
     atLeastOneRepeatedLetter)
|> Array.length