open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day07Input.txt")

let hasABBA (x : string) =
  x 
  |> Seq.windowed 4 
  |> Seq.exists (fun [| a; b; b'; a' |] -> a <> b && a = a' && b = b')

let part1 = 
  input
  |> Seq.map (fun line -> line.Split('[', ']'))
  |> Seq.filter (fun segments ->
    let n = segments.Length
    let outsideBrackets = { 0..2..n-1 } |> Seq.map (fun idx -> segments.[idx])
    let insideBrackets  = { 1..2..n-1 } |> Seq.map (fun idx -> segments.[idx])

    (insideBrackets |> Seq.forall (not << hasABBA)) &&
    (outsideBrackets |> Seq.exists hasABBA)
  )
  |> Seq.length

let getABAs (x : string) =
  x 
  |> Seq.windowed 3
  |> Seq.filter (fun [| a; b; a' |] -> a <> b && a = a')

let hasBAB (BABs : char[][]) (x : string) =
  x
  |> Seq.windowed 3
  |> Seq.exists (fun chars -> BABs |> Array.exists ((=) chars))

let (|ToBABs|) (ABAs : char[][]) =
  ABAs |> Array.map (fun [| a; b; _ |] -> [| b; a; b |])

let part2 =
  input
  |> Seq.map (fun line -> line.Split('[', ']'))
  |> Seq.filter (fun segments ->
    let n = segments.Length
    let outsideBrackets = { 0..2..n-1 } |> Seq.map (fun idx -> segments.[idx])
    let insideBrackets  = { 1..2..n-1 } |> Seq.map (fun idx -> segments.[idx])

    let ABAs = outsideBrackets |> Seq.collect getABAs |> Seq.distinct |> Seq.toArray
    match ABAs with
    | [||] -> false
    | ToBABs BABs ->
      if BABs.Length > 1 then printfn "%A" ABAs
      (insideBrackets |> Seq.exists (hasBAB BABs))
  )
  |> Seq.length