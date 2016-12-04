#load "Day02.fs"

open Day02

let keypad = 
  [|
    [| 1; 2; 3 |]
    [| 4; 5; 6 |]
    [| 7; 8; 9 |]
  |]

let between min max x = x >= min && x <= max
let isValid (row, col) = row |> between 0 2 && col |> between 0 2

let answer = solve isValid keypad
printf "%A" answer