open System
open System.IO

let input = File.ReadAllLines(__SOURCE_DIRECTORY__ + "/Day22Input.txt")

type Node =
  {
    Position : int * int
    Size     : int
    Used     : int
  }

let split (separators : string[]) (x : string) = 
  x.Split(separators, StringSplitOptions.RemoveEmptyEntries)

let nodes =
  input.[2..]
  |> Array.map (split [| " "; "T"; "%" |])
  |> Array.map (fun [| id; size; used; _; _ |] -> 
    let [| x; y |] = id |> split [| "/dev/grid/node"; "x"; "y"; "-" |]

    {
      Position = (int x, int y)
      Size     = int size
      Used     = int used
    })

let byData  = nodes |> Array.sortBy (fun x -> x.Used)
let byAvail = nodes |> Array.sortByDescending (fun x -> x.Size - x.Used)

let part1 =
  byData
  |> Seq.filter (fun x -> x.Used > 0)
  |> Seq.sumBy (fun x ->
    byAvail 
    |> Seq.takeWhile (fun y -> y.Size - y.Used >= x.Used)
    |> Seq.filter (fun y -> y.Position <> x.Position)
    |> Seq.length)

let maxX = nodes |> Array.map (fun n -> fst n.Position) |> Array.max
let maxY = nodes |> Array.map (fun n -> snd n.Position) |> Array.max
let between min max x = x >= min && x <= max

let grid = Array2D.init (maxX + 1) (maxY + 1) (fun x y -> 
  nodes |> Array.find (fun n -> n.Position = (x, y)))

let move (srcX, srcY) (destX, destY) (grid : Node[,]) =
  printfn "moving (%d, %d) to (%d, %d)" srcX srcY destX destY

  let src  = grid.[srcX, srcY]
  let dest = grid.[destX, destY]

  printfn "src [size %d, used %d]  dest [size %d, used %d]" src.Size src.Used dest.Size dest.Used

  if src.Used > (dest.Size - dest.Used)
  then None 
  else 
    let grid' = Array2D.copy grid
    grid'.[srcX, srcY]   <- { src with Used = 0 }
    grid'.[destX, destY] <- { dest with Used = dest.Used + src.Used }
    Some grid'

open System.Collections.Generic

let findPaths (grid : Node[,]) start target =
  let cache = new HashSet<int * int>([start])

  [| ([start], grid) |]
  |> Seq.unfold (fun states ->
    states
    |> Seq.collect (fun ((x, y)::tl, grid) ->
      [ x+1, y; x-1, y; x, y-1; x, y+1 ]
      |> Seq.filter (fun (x', y') -> 
        x' |> between 0 maxX && 
        y' |> between 0 maxY &&
        cache.Add(x', y'))
      |> Seq.choose (fun (x', y') -> 
        move (x', y') (x, y) grid
        |> Option.map (fun grid' -> 
          (x', y')::(x, y)::tl, grid')
      )
    )
    |> Seq.toArray
    |> function 
      | [||] -> None
      | newStates -> Some(newStates, newStates)
  )
  |> Seq.collect id
  |> Seq.filter (fun (path, _) -> path.Head = target)
  |> Seq.sortBy (fun (path, _) -> path.Length)
  |> Seq.head
  |> fst

let path = findPaths grid (7, 17) (35, 0)
let part2 = path.Length + 35 * 5