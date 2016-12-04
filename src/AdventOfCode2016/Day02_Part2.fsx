#load "Day02.fs"

open Day02

let keypad = 
  [|
    [| None;     None;     Some '1'; None;     None     |]
    [| None;     Some '2'; Some '3'; Some '4'; None     |]
    [| Some '5'; Some '6'; Some '7'; Some '8'; Some '9' |]
    [| None;     Some 'A'; Some 'B'; Some 'C'; None     |]
    [| None;     None;     Some 'D'; None;     None     |]
  |]

let between min max x = x >= min && x <= max
let isValid (row, col) = 
  row |> between 0 4 && 
  col |> between 0 4 &&
  keypad.[row].[col] <> None

let answer = solve isValid keypad |> Array.map Option.get
printf "%A" answer