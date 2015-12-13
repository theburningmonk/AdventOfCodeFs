#load "Day02.fs"

open System
open Day02

let split (by : char) (x : string) = 
    x.Split(
        [| by |], 
        StringSplitOptions.RemoveEmptyEntries)

input
|> split '\n'
|> Array.map (split 'x' >> Array.map int)
|> Array.sumBy (fun ([| l; w; h |] as dim) ->
    let [| l'; w' |] = (Array.sort dim).[0..1]
    let wrap = l' + l' + w' + w'
    let bow  = l * w * h
    wrap + bow)