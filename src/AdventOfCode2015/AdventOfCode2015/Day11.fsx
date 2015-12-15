let input = "hepxcrrq"

let (<&&>) f g x = f x && g x

let threeIncrLetters (pwd : string) =
    pwd 
    |> Seq.map int 
    |> Seq.windowed 3
    |> Seq.exists (fun [| a; b; c |] ->
        b = a + 1 && c = b + 1)

let iol = set [ 'i'; 'o'; 'l' ]
let noIOL (pwd : string) =
    pwd |> Seq.forall (iol.Contains >> not)

let atLeastTwoPairs (pwd : string) =
    pwd 
    |> Seq.windowed 2
    |> Seq.filter (fun [| a; b |] -> a = b)
    |> Seq.distinct
    |> Seq.length
    |> fun n -> n >= 2

let isValid = 
    threeIncrLetters <&&>
    noIOL <&&>
    atLeastTwoPairs

let incr (pwd : string) =
    // 'a' = 97, 'z' = 122
    let incrChar (c : char) = 
        match int c + 1 with
        | n when n > 122 -> 'a'
        | n -> char n

    let rec incrAt idx (pwd : string) =
        let pwd =
            pwd.Substring(0, idx)
            + string (incrChar pwd.[idx])
            + pwd.Substring(idx+1)
        
        match pwd.[idx] with
        // char looped around, incr the next char
        | 'a' -> incrAt (idx-1) pwd
        | _   -> pwd

    incrAt (pwd.Length-1) pwd

Seq.unfold (fun pwd -> 
    let nxtPwd = incr pwd
    Some (nxtPwd, nxtPwd)) input
|> Seq.filter isValid
|> Seq.head