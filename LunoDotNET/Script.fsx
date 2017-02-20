open System

let a_parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    else if str.[0] = 'a' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false,str)