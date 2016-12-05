open System
open System.Text
open System.Security.Cryptography

let input = "abbhdwsy"

let md5 = CryptoConfig.CreateFromName("MD5") :?> HashAlgorithm

let hash (input : string) =
  input
  |> Encoding.UTF8.GetBytes
  |> md5.ComputeHash 
  |> Array.map (fun (x : byte) -> String.Format("{0:X2}", x))
  |> String.concat ""

let hashes =
  Seq.initInfinite id
  |> Seq.map (sprintf "%s%d" input)
  |> Seq.map hash
  |> Seq.filter (fun hash -> hash.StartsWith("00000"))

let part1 =
  hashes
  |> Seq.map (fun hash -> hash.[5])
  |> Seq.take 8
  |> fun chars -> new String(chars |> Seq.toArray)

let between min max x = x >= min && x <= max

let part2 =
  hashes
  |> Seq.map (fun hash -> 
    printfn "%A" hash
    hash)
  |> Seq.choose (fun hash -> 
    match Int32.TryParse(hash.Substring(5, 1)) with 
    | true, n -> Some (hash, n)
    | _       -> None) 
  |> Seq.filter (fun (hash, pos) -> pos |> between 0 7)
  |> Seq.scan (fun (hashes, positions : Set<int>) (hash, pos) -> 
      if positions.Contains pos 
      then (hash::hashes, positions.Remove pos)
      else (hashes, positions)
    ) ([], set [0..7])
  |> Seq.skipWhile (fun (_, positions) -> positions.Count > 0)
  |> Seq.head
  |> fst
  |> Seq.sortBy (fun hash -> hash.[5])
  |> Seq.map (fun hash -> hash.[6])
  |> fun chars -> new String(chars |> Seq.toArray)