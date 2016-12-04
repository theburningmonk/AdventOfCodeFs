#load "Day03.fs"

open Day03

input
|> Array.filter isTriangle
|> Array.length