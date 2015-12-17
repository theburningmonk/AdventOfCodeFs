#load "Day15.fs"

open Day15

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)

    if m.Success 
    then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None

type Ingredient =
    {
        Capacity   : int
        Durability : int
        Flavor     : int
        Texture    : int
        Calories   : int
    }

    static member (+) (lf, rt) =
        { 
            Capacity   = lf.Capacity + rt.Capacity
            Durability = lf.Durability + rt.Durability
            Flavor     = lf.Flavor + rt.Flavor
            Texture    = lf.Texture + rt.Texture
            Calories   = lf.Calories + rt.Calories
        }

    static member (*) (x, n) =
        {
            Capacity   = x.Capacity * n
            Durability = x.Durability * n
            Flavor     = x.Flavor * n
            Texture    = x.Texture * n
            Calories   = x.Calories * n
        }

let makeCake (ingredients : (Ingredient * int) seq) =
    ingredients
    |> Seq.map (fun (x, n) -> x * n)
    |> Seq.reduce (+)
    |> fun cake ->
        let score = 
            max 0 cake.Capacity *
            max 0 cake.Durability * 
            max 0 cake.Flavor *
            max 0 cake.Texture
        cake.Calories, score

let parse = function
    | Regex "(\w+): capacity ([-\d]+), durability ([-\d]+), flavor ([-\d]+), texture ([-\d]+), calories (\d+)"
            [ _name; capacity; durability; flavor; texture; calories ]
        -> {
               Capacity   = int capacity
               Durability = int durability
               Flavor     = int flavor
               Texture    = int texture
               Calories   = int calories
           }
    | x -> failwithf "invalid format : %s" x

let ingredients = 
    input.Split '\n' 
    |> Seq.map parse
    |> Seq.toList

let combine ingredients =
    let rec loop ingredients (accN, acc) =
        seq {
            match ingredients with
            | [] -> yield acc
            | [hd] ->
                yield (hd, 100-accN)::acc
            | hd::tl ->
                for n in 0..100-accN do
                    yield! loop tl (accN+n, (hd, n)::acc)
        }
    
    loop ingredients (0, [])

let combinations = combine ingredients

combinations
|> Seq.map makeCake
|> Seq.filter (fst >> (=) 500)
|> Seq.maxBy snd