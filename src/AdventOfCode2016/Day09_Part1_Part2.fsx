open System
open System.IO

let input = File.ReadAllText(__SOURCE_DIRECTORY__ + "/Day09Input.txt")

let (|Int|_|) x = 
  match Int32.TryParse x with
  | true, x -> Some x
  | _       -> None

let (|Repeat|_|) (input : string) = 
  match input.Split('x') with
  | [| Int n; Int count |] -> Some (n, count)
  | _                      -> None

let stitch (chars : char seq) = new String(chars |> Seq.toArray) 

let rec decompressV1 (input : string) = 
  seq {
    // this will give you either
    //    head(nxcount)rest => [| head; "nxcount"; rest |]
    //    (nxcount)rest     => [| ""; "nxcount"; rest |]
    //    text              => [| text |]
    let parts = input.Split([| '('; ')' |], 3)

    match parts with
    | [| text |] -> yield bigint text.Length
    | [| head; Repeat(n, count); rest |] ->
      yield bigint head.Length
      yield bigint n * bigint count
      
      yield! rest |> Seq.skip n |> stitch |> decompressV1
  }

let rec decompressV2 (input : string) = 
  seq {
    // this will give you either
    //    head(nxcount)rest => [| head; "nxcount"; rest |]
    //    (nxcount)rest     => [| ""; "nxcount"; rest |]
    //    text              => [| text |]
    let parts = input.Split([| '('; ')' |], 3)

    match parts with
    | [| text |] -> yield bigint text.Length
    | [| head; Repeat(n, count); rest |] ->
      yield bigint head.Length
      
      let repeatLength = 
        rest 
        |> Seq.take n
        |> stitch
        |> decompressV2
        |> Seq.sum
      
      yield bigint count * repeatLength

      yield! rest |> Seq.skip n |> stitch |> decompressV2
  }

let part1 = decompressV1 input |> Seq.reduce (+)
let part2 = decompressV2 input |> Seq.reduce (+)