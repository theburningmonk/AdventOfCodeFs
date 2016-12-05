open System
open System.IO

let input = 
  File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day04Input.txt")
  |> Array.map (fun line -> 
    let [| rest; checksum |] =
      line.Split('[', ']') 
      |> Array.filter (not << String.IsNullOrWhiteSpace)
    
    let lastDash      = rest.LastIndexOf('-')
    let sectorId      = rest.Substring(lastDash+1) |> int
    let encryptedName = rest.Substring(0, lastDash)

    encryptedName, sectorId, checksum)

let isReal (encryptedName : string) checksum =
  let checksum' =
    encryptedName
    |> Seq.filter ((<>) '-')
    |> Seq.groupBy id
    |> Seq.sortBy (fun (c, cs) -> -(Seq.length cs), c)
    |> Seq.map fst
    |> Seq.take 5
    |> Seq.toArray
    |> fun chars -> new String(chars)

  checksum' = checksum

let realRooms = 
  input |> Array.filter (fun (name, _, checksum) -> isReal name checksum)

let part1 = realRooms |> Array.sumBy (fun (_, sectorId, _) -> sectorId)

let alphabets = [|'a'..'z'|]

let shift c sectorId =
  let oldIdx = Array.findIndex ((=) c) alphabets
  let newIdx = (oldIdx + sectorId) % 26
  alphabets.[newIdx]

let decrypt (encryptedName : string) sectorId = 
  encryptedName
  |> Seq.map (function 
    | '-' -> ' '
    | c   -> shift c sectorId)
  |> fun chars -> new String(chars |> Seq.toArray)

let part2 = 
  realRooms
  |> Seq.map (fun (encryptedName, sectorId, _) -> 
    decrypt encryptedName sectorId, sectorId)
  |> Seq.filter (fst >> (=) "northpole object storage") 
  |> Seq.head
  |> snd