open System.IO

let input = 
    __SOURCE_DIRECTORY__ + "\Day08_Input.txt"
    |> File.ReadAllLines

let (|IsEscaped|_|) = function
    | '\\' :: '\\' :: rest -> Some ('\\', rest)
    | '\\' :: '\"' :: rest -> Some ('\"', rest)
    | '\\' :: 'x' :: x :: y :: rest ->
        let char = sprintf "0x%c%c" x y |> int |> char
        Some (char, rest)
    | _ -> None

let read (input : string) =
    // remove leading and trailing "
    let chars = input.Trim('\"') |> Seq.toList
    
    let rec read acc = function
        | IsEscaped (char, rest) -> read (char::acc) rest
        | hd::rest -> read (hd::acc) rest
        | [] -> acc

    read [] chars

let codeSize = input |> Array.sumBy Seq.length
let memSize  = input |> Array.sumBy (read >> Seq.length)
let answer   = codeSize - memSize