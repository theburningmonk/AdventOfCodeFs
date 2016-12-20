open System.IO
open Checked

let input = 
  File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day20Input.txt")
  |> Seq.map (fun line -> 
    let [| lo; hi |] = line.Split('-')
    int64 lo, int64 hi)
  |> Seq.sortBy fst
  |> Seq.toList

let findAllowedIPs input = 
  let rec loop lo hi input = seq {
    match input with
    | [] -> ()
    | (lo', hi')::tl ->
      if lo' > hi + 1L then 
        yield! { hi+1L .. lo'-1L}
      
      yield! loop lo (max hi hi') tl
  }

  loop 0L 0L input

let part1 = findAllowedIPs input |> Seq.head
let part2 = findAllowedIPs input |> Seq.length