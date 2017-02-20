open System

let toCharList nullableString =
    let notNull = function
        | null -> ""
        | str -> str
    (notNull nullableString) |> List.ofSeq

let charListToString chars = chars |> Array.ofList |> String

let pcharx (charToMatch,str) =    
    match (toCharList str) with
    | first :: remaining ->
        if (first = charToMatch)
        then (sprintf "Found %c" charToMatch, remaining |> charListToString)
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