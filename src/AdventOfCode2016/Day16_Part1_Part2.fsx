#time

let input = "11011110011011101" |> Seq.map ((=) '1') |> Seq.toArray

let solve diskSize input =
  let rec dragonCurve (a : bool[]) =
    if a.Length >= diskSize 
    then a.[0..diskSize-1]
    else  
      [|
        yield! a
        yield false
        yield! a |> Seq.rev |> Seq.map (not)
      |]
      |> dragonCurve

  let rec calcCheckSum (input : bool[]) = 
    let checkSum =
      input
      |> Seq.chunkBySize 2
      |> Seq.map (fun [| x; y |] -> x = y)
      |> Seq.toArray
    
    if checkSum.Length % 2 = 0 then calcCheckSum checkSum else checkSum

  dragonCurve input |> calcCheckSum

//00000100100001100   0.007
let part1 = solve 272 input

//00011010100010010   11.602
let part2 = solve 35651584 input