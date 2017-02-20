open System

let pchar (charToMatch, str) =
    let charListToString x = x |> List.toArray |> System.String
    if String.IsNullOrEmpty(str) then
        let msg = "No more input"
        (msg, "")
    else
        match (str |> Seq.toList) with
        | found :: remaining when found = charToMatch -> 
            let msg = sprintf "Found %c" charToMatch
            (msg, charListToString remaining)
        | first :: _ ->
            let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
            (msg,str)
        | [] ->
            ("Nothing there", str)