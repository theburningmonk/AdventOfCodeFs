type Disc = 
  {
    Number    : int
    Positions : int
    Time0Pos  : int
  }

let solve (discs : Disc list) = 
  // what's the first time we can press and get through the first slot?
  let fstDisc    = discs.[0]
  let timeToZero = fstDisc.Positions - fstDisc.Time0Pos
  let fstT       = timeToZero - fstDisc.Number

  Seq.initInfinite (fun i -> i * fstDisc.Positions + fstT)
  |> Seq.filter (fun t ->
    discs |> List.forall (fun disc ->
      (t + disc.Number - (disc.Positions - disc.Time0Pos)) % disc.Positions = 0))
  |> Seq.head

let input1 = 
  [
    { Number = 1; Positions = 13; Time0Pos = 11 }
    { Number = 2; Positions = 5;  Time0Pos = 0 }
    { Number = 3; Positions = 17; Time0Pos = 11 }
    { Number = 4; Positions = 3;  Time0Pos = 0 }
    { Number = 5; Positions = 7;  Time0Pos = 2 }
    { Number = 6; Positions = 19; Time0Pos = 17 }
  ]
let part1 = solve input1

let input2 = input1 @ [ { Number = 7; Positions = 11; Time0Pos = 0 } ]
let part2  = solve input2