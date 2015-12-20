open System.IO
open System.Text.RegularExpressions

let input = 
    __SOURCE_DIRECTORY__ + "\Day12_Input.txt"
    |> File.ReadAllText

// analogy for 'open' & 'close' is such that you start with
// an open door behind you, you wanna find the matching door
// in front of you to close (i.e. { and })
// so every time you walk through another open door, you gotta
// find the matching door for that to close first, and so on
let findBoundary startingIdx dir open' close' =
    let rec loop idx dir acc =
        if acc = 0 then idx
        else
            let idx  = idx + dir
            let char = input.[idx]
            if char = open' then loop idx dir (acc+1)
            elif char = close' then loop idx dir (acc-1)
            else loop idx dir acc

    loop startingIdx dir 1

let findBoundaryL idx = findBoundary idx -1 '}' '{'
let findBoundaryR idx = findBoundary idx 1 '{' '}'

let bounds = 
    Regex.Matches(input, ":\"red\"")
    |> Seq.cast<Match>
    |> Seq.map (fun m -> 
        let start' = findBoundaryL m.Index
        let end'   = findBoundaryR m.Index

        start', end')
    |> Seq.toArray

Regex.Matches(input, "([-0-9]+)") 
|> Seq.cast<Match>
|> Seq.filter (fun m ->
    not <| Array.exists (fun (start', end') ->
        m.Index > start' && m.Index < end') bounds)
|> Seq.sumBy (fun m -> int m.Value)