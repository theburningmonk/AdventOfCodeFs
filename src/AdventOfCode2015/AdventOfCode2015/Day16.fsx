#load "Day16.fs"

open Day16

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type AuntSue = 
    {
        No  : int
        Memories : Map<string, int>
    }

let parse = function
    | Regex "Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)"
            [ no; mem1; val1; mem2; val2; mem3; val3 ] ->
        {
            No = int no
            Memories =
                [
                    (mem1, int val1)
                    (mem2, int val2)
                    (mem3, int val3)
                ]
                |> Map.ofList
        }
    | input -> failwithf "invalid format : %s" input

let aunts = input.Split '\n' |> Array.map parse
    
let facts =
    [|
        ("children",    3)
        ("cats",        7)
        ("samoyeds",    2)
        ("pomeranians", 3)
        ("akitas",      0)
        ("vizslas",     0)
        ("goldfish",    5)
        ("trees",       3)
        ("cars",        2)
        ("perfumes",    1)
    |]

let find (aunts : AuntSue[]) =
    aunts |> Array.filter (fun sue ->
        facts |> Array.forall (fun (k, v) ->
            match sue.Memories.TryFind k with
            | None    -> true
            | Some v' -> v = v'
        )
    )

find aunts