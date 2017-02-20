open System

let pcharx (charToMatch,str) =
    let removeStringNull = function
        | null -> ""
        | str -> str
    match (removeStringNull str |> List.ofSeq) with
    | first :: remaining ->
        if (first = charToMatch)
        then (sprintf "Found %c" charToMatch, remaining |> Array.ofList |> String)
        else (sprintf "Expecting '%c'. Got '%c'" charToMatch first, str)
    | [] -> ("No more input", "")

let pchar (charToMatch,str) =
    if String.IsNullOrEmpty(str) then
        let msg = "No more input"
        (msg,"")
    else 
        let first = str.[0] 
        if first = charToMatch then
            let remaining = str.[1..]
            let msg = sprintf "Found %c" charToMatch
            (msg,remaining)
        else
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
            (msg,str)