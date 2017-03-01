open System

type Result<'a> =
    | Success of 'a
    | Failure of string

 type Parser<'r> = Parser of (string -> Result<'r * string>)

//-------------------------------------------------------------

let pchar charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else 
            let first = str.[0] 
            if first = charToMatch then
                let remaining = str.[1..]
                let _msg = sprintf "Found %c" charToMatch
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                Failure msg
    Parser innerFn

let run parser str =
    let (Parser innerFn) = parser
    innerFn str

let andThen parser1 parser2 =
    let innerFn input =
        match (run parser1 input) with
        | Failure err -> Failure err
        | Success (value1, remaining1) ->
            match (run parser2 remaining1) with
            | Failure err -> Failure err
            | Success (value2, remaining2) ->
                let newValue = (value1,value2)
                Success (newValue, remaining2)
    Parser innerFn

let (.>>.) = andThen

let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success _ -> result1
        | Failure _ -> run parser2 input            
    Parser innerFn

let (<|>) = orElse

let choice listOfParsers =
    List.reduce (<|>) listOfParsers

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice

let parseLowercase =
    anyOf ['a'..'z']

let parseDigit =
    anyOf ['0'..'9']

let mapP f parser =
    let innerFn input =
        let result = run parser input
        match result with
        | Failure err -> Failure err
        | Success (value, remaining) ->
            let newValue = f value
            Success(newValue, remaining)
    Parser innerFn

let (<!>) = mapP
let (|>>) parser f = mapP f parser

let parse3DigitsAsString = 
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> fun ((c1,c2),c3) -> String [|c1;c2;c3|]

let parse3digitsAsInt =
    parse3DigitsAsString |>> int

let returnP x =
    let innerFn input =
        Success (x, input)
    Parser innerFn

let applyP fP xP =
    fP .>>. xP
    |>> (fun (f,x) -> f x)

let (<*>) = applyP

