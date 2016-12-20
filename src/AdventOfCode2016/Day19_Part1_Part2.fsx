#time

let input = 3018458.0

let logn n m = log10 m / log10 n |> floor |> int
let log2 = logn 2.0
let log3 = logn 3.0

let part1 = 
  let l = input - pown 2.0 (log2 input) |> int
  2 * l + 1

let part2 = 
  let n = log3 input
  let m = pown 3.0 n
  let l = input - m

  if l = 0.0 then m |> int
  elif l <= m then l |> int 
  else m + (l - m) * 2.0 |> int

// type Elf = { N: int; mutable Gifts: int }

// let rec loop (elves: ResizeArray< Elf>)= 
//   let this = elves.[0]
//   let opposite = elves.[elves.Count / 2]

//   this.Gifts <- this.Gifts + opposite.Gifts
//   opposite.Gifts <- 0

//   elves.RemoveAt(elves.Count/2)
//   elves.RemoveAt(0)
//   elves.Add(this)

//   if elves.Count = 1 then elves.[0].N else loop elves

// for i = 3 to 81 do
//   let elves = [| 1..i |] |> Array.map (fun i -> { N=i; Gifts=1 })
//   let ans = loop <| new ResizeArray<Elf>(elves)
//   printfn "%d : %d" i ans