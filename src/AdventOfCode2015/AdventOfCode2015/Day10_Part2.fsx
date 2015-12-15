let input = "3113322113"

let read (input : string) =
    input
    |> Seq.fold (fun acc x ->
        match acc with
        | (n, x')::tl when x = x' ->
            (n+1, x')::tl
        | _ -> (1, x)::acc) []
    |> List.rev
    |> Seq.collect (fun (n, x) ->
        sprintf "%d%c" n x)
    |> fun xs -> System.String.Join("", xs)

{ 1..50 }
|> Seq.fold (fun last _ -> read last) input
|> Seq.length