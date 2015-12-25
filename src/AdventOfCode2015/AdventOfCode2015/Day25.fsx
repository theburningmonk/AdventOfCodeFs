open Checked

let F n =
    let rec f acc = function
        | 1 -> 1 + acc
        | 2 -> 2 + acc
        | n -> f (acc+n-1) (n-1)
    f 0 n
let G x y = F (y+x-1) + (x-1)

let H x y =
    { 1..G x y }
    |> Seq.fold (fun last -> function
        | 1 -> 20151125UL
        | _ -> last * 252533UL % 33554393UL) 0UL

let answer = H 3075 2981