open System.IO
open System.Text.RegularExpressions

let input = 
    __SOURCE_DIRECTORY__ + "\Day12_Input.txt"
    |> File.ReadAllText

let matches = Regex.Matches(input, "([-0-9]+)")
matches 
|> Seq.cast<Match>
|> Seq.sumBy (fun m -> int m.Value)