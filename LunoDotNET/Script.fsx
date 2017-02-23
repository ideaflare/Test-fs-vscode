open System

type Result<'a> =
    | Success of 'a
    | Failure of string

let toCharList nullableString =
    let notNull = function
        | null -> ""
        | str -> str
    (notNull nullableString) |> List.ofSeq

let charListToString = Array.ofList >> String

let pcharx (charToMatch,str) =    
    match (toCharList str) with
    | first :: remaining ->
        if (first = charToMatch)
        then (sprintf "Found %c" charToMatch, remaining |> charListToString)
        else (sprintf "Expecting '%c'. Got '%c'" charToMatch first, str)
    | [] -> ("No more input", "")

let pchar (charToMatch,str) =
    if String.IsNullOrEmpty(str) then
        Failure "No more input"
    else 
        let first = str.[0] 
        if first = charToMatch then
            let remaining = str.[1..]
            let msg = sprintf "Found %c" charToMatch
            Success (charToMatch, remaining)
        else
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
            Failure msg