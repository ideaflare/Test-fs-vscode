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
    if String.IsNullOrEmpty(str) then
        (false, "")
    else match (str |> Seq.toList) with
        | 'B' :: rest -> (true, System.String rest)
        | no_b_found -> (false, System.String no_b_found)