open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines (__SOURCE_DIRECTORY__ + "/Day10Input.txt")

type Target =
  | Bot     of int
  | Output  of int

let (|Regex|_|) pattern input =
  let m = Regex.Match(input, pattern)
  if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
  else None

let (|Int|) x  = Int32.Parse x
let (|Target|) = function | "bot" -> Bot | _ -> Output

type Bot = 
  {
    Number     : int
    Microchips : int list
    LowTarget  : Target
    HiTarget   : Target
  }

let events =
  let bots = new Dictionary<int, Bot>()

  input
  |> Array.sort
  |> Array.iter (fun line ->
    match line with
    | Regex 
        "bot (\d*) gives low to (bot|output) (\d*) and high to (bot|output) (\d*)" 
        [ Int botN; Target lowT; Int lowN; Target hiT; Int hiN ] ->
      let bot = 
        { 
          Number     = botN
          Microchips = []
          LowTarget  = lowT lowN
          HiTarget   = hiT hiN 
        }
      bots.Add(botN, bot)

    | Regex "value (\d*) goes to bot (\d*)" [ Int value; Int botN ] ->
      let bot = bots.[botN]
      bots.[botN] <- { bot with Microchips = value::bot.Microchips })

  let give value = function
    | Bot botN -> 
      let bot = bots.[botN] 
      bots.[botN] <- { bot with Microchips = value::bot.Microchips }
    | _ -> ()

  let botsWithTwo = 
    bots 
    |> Seq.map (fun kvp -> kvp.Value)
    |> Seq.filter (fun bot -> bot.Microchips.Length = 2)

  seq {
    while Seq.length botsWithTwo > 0 do
      for bot in (Array.ofSeq botsWithTwo) do
        let [ lowVal; hiVal ] = bot.Microchips |> List.sort
        bots.[bot.Number] <- { bot with Microchips = [] }
        
        give lowVal bot.LowTarget
        give hiVal bot.HiTarget

        yield bot.Number, (bot.LowTarget, lowVal), (bot.HiTarget, hiVal)
  }

let part1 = 
  events 
  |> Seq.pick (fun (botN, (_, lowVal), (_, hiVal)) ->
    if lowVal = 17 && hiVal = 61 then Some botN else None)

let part2 =
  let isOutput0To2 = function
    | Output 0 | Output 1 | Output 2 -> true
    | _ -> false  

  events
  |> Seq.collect (fun (botN, (lowTarget, lowVal), (hiTarget, hiVal)) ->
     seq {
       if isOutput0To2 lowTarget then yield lowVal
       if isOutput0To2 hiTarget then yield hiVal
     })
  |> Seq.take 3
  |> Seq.reduce (*)