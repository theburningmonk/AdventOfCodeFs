#load "Day09.fs"

open System
open Day09

type Graph =
    {
        Cities : Set<string>
        Connections : Map<string, Map<string, int>>
    }
    static member Default =
        {
            Cities = set []
            Connections = Map.empty<string, Map<string, int>>
        }

let addConn (a, b, distance) (g : Graph) =
    let newConn = 
        match g.Connections.TryFind a with
        | None ->
            g.Connections.Add(a, Map.ofList [ b, distance ])
        | Some m ->
            g.Connections.Add(a, m.Add(b, distance))

    { g with Cities = g.Cities.Add(a).Add(b)
             Connections = newConn }

let parse (line : string) = 
    line.Split(
        [| " to "; " = " |], 
        StringSplitOptions.RemoveEmptyEntries)
    |> function [| a; b; dist |] -> a, b, int dist

let graph = 
    input.Split '\n' 
    |> Array.map parse
    |> Array.fold (fun g (a, b, dist) ->
        addConn (a, b, dist) g
        |> addConn (b, a, dist)) Graph.Default

let findPaths graph = 
    let rec traverse (city::_ as route) (toGo : Set<string>) =
        seq {
            if toGo.IsEmpty then
                yield route
            else 
                let nextCities =
                    toGo
                    |> Set.filter graph.Connections.[city].ContainsKey

                for next in nextCities do
                    yield! traverse (next::route) (toGo.Remove next)
        }

    graph.Cities
    |> Seq.collect (fun start -> 
        traverse [start] (graph.Cities.Remove start))

let calcDistance cities =
    cities 
    |> Seq.windowed 2
    |> Seq.sumBy (fun [| a; b |] -> 
        graph.Connections.[a].[b])

findPaths graph 
|> Array.ofSeq
|> Array.map calcDistance
|> Array.max