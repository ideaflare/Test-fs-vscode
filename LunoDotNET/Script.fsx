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
                let msg = sprintf "Found %c" charToMatch
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

let parseA = pchar 'A'
let parseB = pchar 'B'
let parseC = pchar 'C'
let parseAThenB = parseA .>>. parseB
let parseBOrC = parseB <|> parseC
let aAndThenBOrC = parseA .>>. parseBOrC
run aAndThenBOrC "AcCAZOOKA"