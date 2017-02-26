#load "Script.fsx"
open ParseEx
open System

let toCharList nullableString =
    let notNull = function
        | null -> ""
        | str -> str
    (notNull nullableString) |> List.ofSeq

let charListToString = Array.ofList >> String

let pcharx charToMatch =
    fun str ->    
    match (toCharList str) with
    | first :: remaining ->
        if (first = charToMatch)
        then Success (charToMatch, remaining |> charListToString)
        else Failure (sprintf "Expecting '%c'. Got '%c'" charToMatch first)
    | [] -> Failure "No more input"

