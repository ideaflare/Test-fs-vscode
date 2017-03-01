#load "Script.fsx"
// If you don't declare a module in fsx file, it implicitly creates a module the same name as file name. (Title Case)
// http://stackoverflow.com/a/9545564
open Script
open System

let toCharList = function
        | null -> []
        | str -> str |> List.ofSeq

let charListToString = Array.ofList >> String

let pcharx charToMatch =
    fun str ->    
    match (toCharList str) with
    | first :: remaining ->
        if (first = charToMatch)
        then Success (charToMatch, remaining |> charListToString)
        else Failure (sprintf "Expecting '%c'. Got '%c'" charToMatch first)
    | [] -> Failure "No more input"

