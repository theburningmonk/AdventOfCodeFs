﻿let findAllDivisors n =
    let max = float n |> sqrt |> int
    { 1..max } 
    |> Seq.filter (fun n' -> n % n' = 0)
    |> Seq.collect (fun n' -> [ n'; n / n' ])
    |> Seq.distinct
    |> Seq.toArray

let totalGifts = findAllDivisors >> Array.sumBy ((*) 10)

let input = 29000000

100000
|> Seq.unfold (fun n -> Some(n+1, n+1))
|> Seq.filter (fun n -> totalGifts n >= input)
|> Seq.head