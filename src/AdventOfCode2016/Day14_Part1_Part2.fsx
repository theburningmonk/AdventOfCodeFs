#time

open System
open System.Collections.Generic
open System.Text
open System.Security.Cryptography

let input = "ahsbgdzn"

let md5 = CryptoConfig.CreateFromName("MD5") :?> HashAlgorithm

let memoize f =
  let dict = new Dictionary<_,_>()
  fun n ->
    match dict.TryGetValue(n) with
    | true, v -> v
    | _ ->
      let temp = f(n)
      dict.Add(n, temp)
      temp

let hash = 
  let f (input : string) =
    let bytes = input |> Encoding.UTF8.GetBytes |> md5.ComputeHash
    BitConverter.ToString(bytes).Replace("-", "").ToLower()

  memoize f

let inline findSequencesOf n (input : string) =
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

let part2 = solve keyStretch