open System
open System.IO

let inputs = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/Day12Input.txt")

let (|Int|_|) x = 
  match Int32.TryParse x with
  | true, n -> Some n
  | _       -> None

let execute initValues (inputs : string[]) =
  let registers = new System.Collections.Generic.Dictionary<string, int>()
  initValues |> Seq.iter (fun (key, value) -> registers.[key] <- value)

  let inputs = inputs |> Array.map (fun x -> x.Split())

  let rec loop idx = 
    if idx >= inputs.Length then registers
    else 
      match inputs.[idx] with
      | [| "cpy"; Int n; dest |] -> 
        registers.[dest] <- n
        loop (idx+1)
      | [| "cpy"; src; dest   |] -> 
        registers.[dest] <- registers.[src]
        loop (idx+1)
      | [| "inc"; reg |] ->
        registers.[reg] <- registers.[reg] + 1
        loop (idx+1)
      | [| "dec"; reg |] ->
        registers.[reg] <- registers.[reg] - 1
        loop (idx+1)
      | [| "jnz"; Int n; Int offset |] when n <> 0 ->
        loop (idx+offset)
      | [| "jnz"; reg; Int offset |] ->
        match registers.TryGetValue reg with
        | true, n when n <> 0 ->
          // printfn "%A" (registers |> Seq.map (fun (KeyValue(k, v)) -> k, v) |> Seq.toArray)  
          loop (idx+offset)
        | _ -> loop (idx+1)
      | _ -> loop (idx+1)

  loop 0

let part1 = (execute [] inputs).["a"]
let part2 = (execute [ "c", 1 ] inputs).["a"]