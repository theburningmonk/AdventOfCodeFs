#time

open System

let input = "^.^^^..^^...^.^..^^^^^.....^...^^^..^^^^.^^.^^^^^^^^.^^.^^^^...^^...^^^^.^.^..^^..^..^.^^.^.^......."

let previousTiles (row : string) =
  seq {
    yield [| '.'; row.[0]; row.[1] |]
    yield! row |> Seq.windowed 3
    yield [| row.[row.Length-2]; row.[row.Length-1]; '.' |]
  }

let genRows input =
  let nextRow = function 
    | [| '^'; '^'; '.' |]
    | [| '^'; '.'; '.' |]
    | [| '.'; '^'; '^' |]
    | [| '.'; '.'; '^' |] -> '^'
    | _                   -> '.'

  seq {
    yield input
    yield! input |> Seq.unfold (fun row ->
      let nextRow = 
        row 
        |> previousTiles 
        |> Seq.map nextRow
        |> Seq.toArray
        |> fun chars -> new String(chars)
      Some(nextRow, nextRow))
  }

let solve input n =
  genRows input
  |> Seq.take n 
  |> Seq.sumBy (fun row -> row |> Seq.filter ((=) '.') |> Seq.length)

let part1 = solve input 40
let part2 = solve input 400000