#time

open System
open System.Collections.Generic
open System.Text
open System.Security.Cryptography

let input = "ahsbgdzn"

let md5 = CryptoConfig.CreateFromName("MD5") :?> HashAlgorithm
let hex = 
  seq {
    yield! Seq.zip { 0uy..9uy } { '0'..'9' }
    yield! Seq.zip { 10uy..15uy } { 'a'..'f' }
  }
  |> Map.ofSeq

let hash (input : string) =
  let bytes = input |> Encoding.UTF8.GetBytes |> md5.ComputeHash
  let chars = Array.zeroCreate<char> (bytes.Length * 2)
  bytes |> Seq.iteri (fun i b -> 
    chars.[i*2] <- hex.[b / 16uy]
    chars.[i*2+1] <- hex.[b % 16uy])

  new String(chars)

let findSequencesOf n (input : string) =
  input
  |> Seq.windowed n
  |> Seq.choose (fun chars -> 
    if chars |> Seq.forall ((=) chars.[0]) then Some chars.[0] else None)

let solve (hash : string -> string) =
  let hashes =
    Seq.initInfinite id
    |> Seq.map (fun idx -> idx, sprintf "%s%d" input idx |> hash)
    |> Seq.cache

  let sequencesOfFive = new Dictionary<char, int list>()
  hashes 
  |> Seq.take 25000
  |> Seq.iter (fun (idx, h) ->
    findSequencesOf 5 h
    |> Seq.iter (fun c -> 
      match sequencesOfFive.TryGetValue c with
      | true, indices -> sequencesOfFive.[c] <- idx::indices
      | _             -> sequencesOfFive.[c] <- [ idx ])
  )

  let validHashes =
    hashes
    |> Seq.filter (fun (idx, hs) -> 
      match findSequencesOf 3 hs |> Seq.tryHead with
      | None   -> false
      | Some c -> 
        match sequencesOfFive.TryGetValue c with
        | true, indices -> 
          indices |> List.exists (fun idx' -> idx' > idx && idx' <= idx + 1000)
        | _ -> false
    )

  validHashes |> Seq.skip 63 |> Seq.head |> fst

let part1 = solve hash

let keyStretch (input : string) =
  { 0..2016 } |> Seq.fold (fun last _ -> hash last) input

keyStretch (input + "1")

let part2 = solve keyStretch