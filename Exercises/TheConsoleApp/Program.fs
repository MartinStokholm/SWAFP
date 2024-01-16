open FParsec

// Define the type for the output
type Output = {
    ints: int list list
    parsedLines: int
}

// Full input for parsing
let inputText =
    "10 -3 3 0\n\
    4 -4 2 0\n\
    1 2 3 4 5 0\n "

let addLine (line: int list) (stream: CharStream<Output>) =
    let output = stream.UserState
    { output with ints = line :: output.ints; parsedLines = output.parsedLines + 1 }
    |> Reply.Success
   

// Find a parser that parses a non-zero number from the input text
// answer is pint32 

// Create a parser that parses a number + a number of whitespaces
let pNumber_ws = pint32 .>> spaces
 
// Use your parsers above when the char is not a ‘0’
let pNumber_ws_notZero = manyTill pNumber_ws (pchar '0') .>> newline >>= addLine 

// Create a parsers that parses a number of lines - so till the end of file (eof)

// Example usage

let output = run pNumber_ws_notZero "34 1 -2 0"
    
printfn $"{output}"

