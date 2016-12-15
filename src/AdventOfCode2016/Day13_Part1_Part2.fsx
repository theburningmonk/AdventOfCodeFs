#time

open System.Collections.Generic
open Checked

let input = 1362

// taken from http://www.necessaryandsufficient.net/2009/04/optimising-bit-counting-using-iterative-data-driven-development/
let countSetBits n =
  let n = n - ((n >>> 1) &&& 0x55555555)
  let n = (n &&& 0x33333333) + ((n >>> 2) &&& 0x33333333)
  let n = n + ((n >>> 4) &&& 0x0F0F0F0F)
  let n = n + (n >>> 8)
  let n = n + (n >>> 16)
  (n &&& 0x0000003F)

let memoize f =
  let dict = new Dictionary<_,_>()
  fun n ->
    match dict.TryGetValue(n) with
    | true, v -> v
    | _ ->
      let temp = f(n)
      dict.Add(n, temp)
      temp

let isOpenSpace = 
  let f (x, y) = 
    let n = x*x + 3*x + 2*x*y + y + y*y + input
    countSetBits n % 2 = 0

  memoize f

let (<&&>) f g = fun x -> f(x) && g(x)

let travel start = 
  let visited = new HashSet<int*int>([ start ])

  let filters = 
    (fun (x, y) -> x >= 0 && y >= 0) <&&> 
    visited.Add <&&> 
    isOpenSpace

  (0, [ start ])
  |> Seq.unfold (fun (moves, ps) -> 
    let nextPs = 
      ps 
      |> Seq.collect (fun (x, y) -> [ (x+1, y); (x-1, y); (x, y-1); (x, y+1) ])
      |> Seq.distinct
      |> Seq.filter filters
      |> Seq.toList

    let nextState = moves+1, nextPs
    Some(nextState, nextState))
  
let part1 = 
  travel (1, 1)
  |> Seq.filter (fun (_, ps) -> ps |> List.exists ((=) (31, 39)))
  |> Seq.head
  |> fst

let part2 = 
  travel (1, 1)
  |> Seq.take 50
  |> Seq.map snd
  |> Seq.sumBy Seq.length
  |> (+) 1 // the starting position