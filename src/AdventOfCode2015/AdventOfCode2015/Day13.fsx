#load "Day13.fs"

open Day13

let parse (line : string) =
    // e.g. Alice would gain 54 happiness units by sitting next to Bob.
    let tokens = line.TrimEnd().TrimEnd('.').Split ' '
    let x, y = tokens.[0], tokens.[10]
    let unit = int tokens.[3]
    let dir  = if tokens.[2] = "gain" then 1 else -1
    x, y, unit * dir

type Graph = Map<string, Map<string, int>>

let connections = 
    input.Split '\n' 
    |> Seq.map parse
    |> Seq.fold (fun (g : Graph) (x, y, change) -> 
        match g.TryFind x with
        | Some g' -> g.Add(x, g'.Add(y, change))
        | _ -> g.Add(x, Map.ofList [ (y, change) ])
    ) Map.empty<string, Map<string, int>>

module Map =
    let keys (map : Map<'a, 'b>) = 
        map |> Seq.map (fun (KeyValue(k, _)) -> k)

let arrangements =
    let rec combos acc rest =
        seq {
            match rest with
            | [] -> yield acc |> List.toArray
            | _  -> 
                for nxt in rest do
                    let acc  = nxt::acc
                    let rest = List.except [nxt] rest
                    yield! combos acc rest
        }

    combos [] <| List.ofSeq (Map.keys connections)

let score (arrangement : string[]) =
    let leftOf idx  = 
        match idx - 1 with
        | -1   -> arrangement.Length-1
        | idx' -> idx'

    let rightOf idx = 
        match idx + 1 with
        | idx' when idx' = arrangement.Length -> 0
        | idx' -> idx'

    arrangement 
    |> Array.mapi (fun idx a ->
        let left  = arrangement.[leftOf idx]
        let right = arrangement.[rightOf idx]
        connections.[a].[left] + connections.[a].[right])
    |> Array.sum
   
arrangements |> Seq.map score |> Seq.max