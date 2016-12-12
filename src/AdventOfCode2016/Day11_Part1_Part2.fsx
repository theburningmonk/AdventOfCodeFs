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

  override x.ToString() =
    let byElement = 
      x.Floors 
      |> Seq.mapi (fun n items -> items |> Seq.map (fun i -> n, i))
      |> Seq.collect id
      |> Seq.groupBy (fun (_n, i) -> i.Element)
      |> Seq.map (fun (_e, itemOnFloors) -> 
        match itemOnFloors |> Seq.toArray with
        | [| (i, Generator _); (j, _) |] -> i, j
        | [| (i, Microchip _); (j, _) |] -> j, i)
      |> Seq.sort

    sprintf "%d-%s" x.Elevator <| System.String.Join("", byElement)

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

let solve initState =
  let visitedStates = new System.Collections.Generic.HashSet<string>()

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
    |> Seq.filter (fun state -> visitedStates.Add(state.ToString()))
    |> Seq.filter (function | Failed -> false | _ -> true)

  (0, [| initState |])
  |> Seq.unfold (fun (moves, states) ->
    printfn "has tried out %d states" visitedStates.Count
    let nextStates = states |> Seq.collect next |> Seq.toArray
    let nextItem = moves+1, nextStates
    Some (nextItem, nextItem))
  |> Seq.choose (fun (moves, states) ->
    let successStates = 
      states |> Array.filter (function | Success -> true | _ -> false)
    if successStates.Length > 0 then Some (moves, successStates) else None)
  |> Seq.head
  |> fst

let initFloorsPart1 = 
  [| 
    set [ Generator "promethium"; Microchip "promethium" ]
    set [ Generator "cobalt"; Generator "curium"; 
          Generator "ruthenium"; Generator "plutonium" ]
    set [ Microchip "cobalt"; Microchip "curium"; 
          Microchip "ruthenium"; Microchip "plutonium" ]
    set []
  |]

let part1 = solve <| { Elevator = 0; Floors = initFloorsPart1 }

let initFloorsPart2 =
  [| 
    set [ Generator "promethium"; Microchip "promethium"; 
          Generator "elerium";    Microchip "elerium";
          Generator "dilithium";  Microchip "dilithium" ]
    set [ Generator "cobalt"; Generator "curium"; 
          Generator "ruthenium"; Generator "plutonium" ]
    set [ Microchip "cobalt"; Microchip "curium"; 
          Microchip "ruthenium"; Microchip "plutonium" ]
    set []
  |]

let part2 = solve <| { Elevator = 0; Floors = initFloorsPart2 }