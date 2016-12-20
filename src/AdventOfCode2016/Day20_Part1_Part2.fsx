open System.IO

let input = 
  File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day20Input.txt")
  |> Seq.map (fun line -> 
    let [| lo; hi |] = line.Split('-')
    uint32 lo, uint32 hi)
  |> Seq.sortBy fst
  |> Seq.toList

let part1 = 
  let rec loop lo hi = function
    | [] -> hi + 1u
    | (lo', hi')::tl ->
      if lo' <= hi + 1u then loop lo (max hi hi') tl else hi + 1u
  
  loop 0u 0u input
