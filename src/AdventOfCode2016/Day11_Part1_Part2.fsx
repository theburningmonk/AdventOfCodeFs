type Item =
  | Generator of string
  | Microchip of string

  member x.Element =
    match x with
    | Generator e -> e
    | Microchip e -> e

type Floor = Set<Item>

type State =
  {
    Elevator : int
    Floors   : Floor[]
  }

let initFloors = 
  [| 
    set [ Generator "promethium"; Microchip "promethium" ]
    set [ Generator "cobalt"; Generator "curium"; Generator "ruthenium"; Generator "plutonium" ]
    set [ Microchip "cobalt"; Microchip "curium"; Microchip "ruthenium"; Microchip "plutonium" ]
    set []
  |]

let initState = { Elevator = 0; Floors = initFloors }

let (|Empty|NotEmpty|) (floor : Floor) = 
  if floor.Count = 0 then Empty else NotEmpty

let (|Fried|_|) (floor : Floor) =
  let chips, generators = 
    floor 
    |> Set.partition (function | Microchip _ -> true | _ -> false)
    |> fun (chips, gens) ->
      chips |> Set.map (fun x -> x.Element),
      gens  |> Set.map (fun x -> x.Element)

  let unmatchedChips = Set.difference chips generators

  // any microchips that are end up matching to other incompatible RTGs 
  // will be fried
  if unmatchedChips.Count > 0 && generators.Count > 0
  then Some unmatchedChips
  else None

let (|Success|Failed|InProgress|) { Floors = floors } =
  match floors with
  | [| Empty; Empty; Empty; NotEmpty |] -> Success
  | [| Fried _; _; _; _ |]
  | [| _; Fried _; _; _ |]
  | [| _; _; Fried _; _ |]
  | [| _; _; _; Fried _ |] -> Failed
  | _ -> InProgress

open System.Collections.Generic
let visitedStates = new HashSet<State>()

let next { Elevator = floorNum; Floors = floors } =
  // all the different ways items can be loaded into the elevator
  let itemCombos (floor : Floor) =
    seq {
      for item in floor do
        yield set [ item ]
        for otherItem in floor.Remove item do
          yield set [ item; otherItem ]
    }
    |> Seq.distinct

  let moveItems oldFloor newFloor items =
    let floors' = Array.copy floors
    floors'.[oldFloor] <- Set.difference floors.[oldFloor] items
    floors'.[newFloor] <- Set.union floors.[newFloor] items
    floors'

  seq { 
    let floor = floors.[floorNum]
    
    for items in itemCombos floor do
      if floorNum >= 1 then
        let floorNum' = floorNum-1
        let floors'   = moveItems floorNum floorNum' items
        yield { Elevator = floorNum'; Floors = floors' }

      if floorNum < 3 then
        let floorNum' = floorNum+1
        let floors'   = moveItems floorNum floorNum' items
        yield { Elevator = floorNum'; Floors = floors' }
  }
  |> Seq.filter (fun state -> visitedStates.Add(state))
  |> Seq.filter (function | Failed -> false | _ -> true)

let successMoves =
  (0, [| initState |])
  |> Seq.unfold (fun (moves, states) ->
    let nextStates = states |> Seq.collect next |> Seq.toArray
    let nextItem = moves+1, nextStates
    Some (nextItem, nextItem))
  |> Seq.choose (fun (moves, states) ->
    let successStates = 
      states |> Array.filter (function | Success -> true | _ -> false)
    if successStates.Length > 0 then Some (moves, successStates) else None)

let part1 = successMoves |> Seq.head |> fst

// successMoves |> Seq.take 10 |> Seq.toArray