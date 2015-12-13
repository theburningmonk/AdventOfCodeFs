#r "System.Security"

let md5 = System.Security.Cryptography.MD5.Create()

Seq.unfold (fun n -> Some (n+1, n+1)) 0
|> Seq.map (fun n ->
    let hash = 
        sprintf "bgvyzdsv%d" n
        |> System.Text.Encoding.UTF8.GetBytes
        |> md5.ComputeHash
    n, hash)
|> Seq.filter (fun (_, bin) ->
    // take advantage of binary-to-hex conversion (4 bits = 1 hex decimal)
    // first 5 hexdecimal = 0 means 
    //      first 2 bytes == 0uy
    //      3rd byte < 16uy
    bin.[0] = 0uy && bin.[1] = 0uy && bin.[2] < 16uy)
|> Seq.head
|> fst