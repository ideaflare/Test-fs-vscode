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

let bindP f p =
    let innerFn input =
        let result1 = run p input
        match result1 with
        | Failure err -> Failure err
        | Success (value, remainingInput) ->
            let p2 = f value
            run p2 remainingInput
    Parser innerFn

(>>=) p f = bindP f p

let returnP x =
    let innerFn input =
        Success (x, input)
    Parser innerFn

let _andThen p1 p2 =
    p1 >>== (fun r1 ->
    p2 >>== (fun r2 ->
    returnP (r1,r2)))

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

let parse3digitsAsInt = 
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> fun ((c1,c2),c3) -> String [|c1;c2;c3|]
    |>> int

let applyP fP xP =
    fP .>>. xP
    |>> (fun (f,x) -> f x)

let (<*>) = applyP

let lift2 f xP yP =
    returnP f <*> xP <*> yP
    
let lx2 f xP yP =
    (applyP ( applyP (returnP f) xP) yP)

let liift f xP yP =
    let fP = returnP f // changes + to Parser<+>
    let i1 = applyP fP xP // changes Parser<+> x to Parser<+ x>
    applyP i1 yP // changes Parser <+ x> Parser<y> to Parser<z> ie resut of (x+y)

let liiift f xP yP =
    let fP = Parser (fun input -> Success (f, input))
    let fxP = fP .>>. xP |>> (fun (plus, x) -> plus x)
    fxP .>>. yP |>> (fun (plusX, y) -> plusX y)

let addP = lift2 (+)

let startsWith (str:string) prefix =
    str.StartsWith(prefix)

let startsWithP = lift2 startsWith  

let rec sequence parserList =
    let cons head tail = head :: tail
    let consP = lift2 cons
    match parserList with
    | [] -> returnP []
    | head :: tail -> consP head (sequence tail)

let parsers = [pchar 'A';pchar 'B';pchar 'C']
let combined = sequence parsers

run combined "ABCD"

let charListToString = Array.ofList >> String

let pstring str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToString

let parseABC = pstring "ABC"
run parseABC "TEST"
run parseABC "ABCDEF"

let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Failure err -> ([], input)
    | Success (firstValue, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

let many parser =
    let rec innerFn input =
        Success (parseZeroOrMore parser input)
    Parser innerFn

let many1 parser =
    let rec innerFn input =
        let firstResult = run parser input
        match firstResult with
        | Failure err -> Failure err
        | Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue :: subsequentValues
            Success (values, remainingInput)
    Parser innerFn

let digit = anyOf ['0'..'9']
let digits = many1 digit
run digits "34431test"
run digits "t"

let pint =
    let resultToInt digitList = String(List.toArray digitList) |> int
    digits |>> resultToInt

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let charListToInt = Array.ofList >> String >> int

let pintNeg =
    let resultToInt (sign, chars) =
        let i = chars |> charListToInt
        match sign with
        | Some _ -> -i
        | None -> i
    opt (pchar '-') .>>. digits
    |>> resultToInt

let (>>.) p1 p2 = p1 .>>. p2 |>> (fun (a, b) -> b)
let (.>>) p1 p2 = p1 .>>. p2 |>> (fun (a, b) -> a)

let whitespace = many1 (anyOf [' ';'\t';'\n'])

let ab = pstring "ab"
let cd = pstring "cd"
let ab_cd = (ab .>> whitespace) .>>. cd

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let sepBy1 p sep =
    let sepThenP = sep >>. p1
    p .>>. many sepThenP
    |>> (fun (p, pList) -> p :: pList)

let sepBy p sep =
    sepBy1 p sep <|> returnP []
