open System

let A_Parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    else if str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false,str)

let B_Parser str =
    let charListToString x = x |> List.toArray |> System.String
    if String.IsNullOrEmpty(str) then
        (false, "")
    else
        match (str |> Seq.toList) with
        | 'B' :: rest -> (true, charListToString rest)
        |  xs -> (false, charListToString xs)