open System
open System.IO

let input = 
    __SOURCE_DIRECTORY__ + "\Day08_Input.txt"
    |> File.ReadAllLines

let encode (input : string) =
    let rec encode acc = function
        | '\"' :: rest -> encode ('\\'::'\"'::acc) rest
        | '\\' :: rest -> encode ('\\'::'\\'::acc) rest
        | hd :: rest   -> encode (hd::acc) rest
        | [] -> acc
    
    [ 
        yield '\"'
        yield! input |> Seq.toList |> encode []
        yield '\"'
    ]

let codeSize = input |> Array.sumBy Seq.length
let memSize  = input |> Array.sumBy (encode >> Seq.length)
let answer   = memSize - codeSize