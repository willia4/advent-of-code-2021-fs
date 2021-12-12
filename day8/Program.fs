open CommonHelpers

type EncodedSymbol = string
type DecodedSymbol = string

type DataRecord = { Inputs: string seq; Outputs: string seq }

let sortLettersInString (s: string) = String.concat "" (s |> Helpers.charStrings |> Seq.sort)

let notExists v s = s |> Seq.exists (fun x -> x = v) |> not

let symbolsForPatternsOfLength length (allPatterns: string seq): seq<seq<EncodedSymbol>> =
    allPatterns
        |> Seq.filter (fun s -> s.Length = length)
        |> Seq.map Helpers.charStrings

let symbolsForDigitOne (allPatterns: string seq) =
    // we can find the patterns for one because they will be the only patterns of length 2
    allPatterns
    |> symbolsForPatternsOfLength 2
    |> Seq.head

let symbolsForDigitSeven (allPatterns: string seq) =
    // we can find the patterns for seven because they will be the only patterns of length 3
    allPatterns
    |> symbolsForPatternsOfLength 3
    |> Seq.head

let symbolsForDigitFour (allPatterns: string seq) =
    // we can find the patterns for four because they will be the only patterns of length 4
    allPatterns
    |> symbolsForPatternsOfLength 4
    |> Seq.head
        
let symbolsForDigitEight (allPatterns: string seq) =
    // we can find the patterns for eight because they will be the only patterns of length 7
    allPatterns
    |> symbolsForPatternsOfLength 7
    |> Seq.head
    
let getEncodedSymbolForDecodedA (allPatterns : string seq): EncodedSymbol =
    // the symbols for one are "c" and "f".
    // the symbols for seven are "c", "f", and "a".
    // thus, the symbol for "a" is the symbol in the seven pattern that is not in the one pattern
    let symbolsForDigitOne = symbolsForDigitOne allPatterns
    let symbolsForDigitSeven = symbolsForDigitSeven allPatterns
        
        
    symbolsForDigitSeven
    |> Seq.filter (fun s -> notExists s symbolsForDigitOne)
    |> Seq.head
    
let getEncodedSymbolForDecodedB (allPatterns: string seq): EncodedSymbol =
    let symbolsForDigitOne = symbolsForDigitOne allPatterns
    let symbolsForDigitFour = symbolsForDigitFour allPatterns
    
    // the symbols for four are "b", "c", "d", and "f"
    // the symbols for one are "c" and "f"
    // so owe know that "b" and "d" are the symbols from four that are not in one 
    let bd = symbolsForDigitFour
             |> Seq.filter (fun s -> notExists s symbolsForDigitOne)
    
    // there are three digits with a pattern length of 6.
    // From "b" and "d", only "b" is in all of them 
    let itemsOfLengthSix = symbolsForPatternsOfLength 6 allPatterns
    bd
    |> Seq.filter (fun s ->
        itemsOfLengthSix |> Seq.forall (Seq.contains s))
    |> Seq.head

let getEncodedSymbolForDecodedD (allPatterns: string seq): EncodedSymbol =
    let symbolsForDigitOne = symbolsForDigitOne allPatterns
    let symbolsForDigitFour = symbolsForDigitFour allPatterns
    
    // the symbols for four are "b", "c", "d", and "f"
    // the symbols for one are "c" and "f"
    // so owe know that "b" and "d" are the symbols from four that are not in one 
    let bd = symbolsForDigitFour
             |> Seq.filter (fun s -> notExists s symbolsForDigitOne)
    
    // there are three digits with a pattern length of 6.
    // From "b" and "d", only "b" is in all of them so "d" is _not_ in all of them
    let patternsOfLengthSix = symbolsForPatternsOfLength 6 allPatterns
    bd
    |> Seq.filter (fun s ->
        patternsOfLengthSix |> Seq.forall (Seq.contains s) |> not)
    |> Seq.head

let getEncodedSymbolsForFandG (allPatterns: string seq): EncodedSymbol seq =
    
    let encodedA = getEncodedSymbolForDecodedA allPatterns
    let encodedB = getEncodedSymbolForDecodedB allPatterns
    let encodedD = getEncodedSymbolForDecodedD allPatterns
    
    // there is one pattern of length 6 that contains both "b" and "d". It happens to be for the digit five.
    let patternsOfLengthFive = symbolsForPatternsOfLength 5 allPatterns
    let symbolsForDigitFive =
        patternsOfLengthFive
        |> Seq.filter (Seq.contains encodedB)
        |> Seq.filter (Seq.contains encodedD)
        |> Seq.head

    // five has "a", "b", 'd", "f", and "g". If we remove "a", "b", and "d" (we know them), that leaves "f" and "g"        
    symbolsForDigitFive
    |> Seq.except [encodedA; encodedB; encodedD ]

let getEncodedSymbolForDecodedC (allPatterns: string seq): EncodedSymbol =
    let fg = getEncodedSymbolsForFandG allPatterns

    // the symbols for one has length 2. One of them is "c" and the other is "f".
    // So whichever symbol is not in fg must be "c"
    symbolsForDigitOne allPatterns
    |> Seq.filter (fun s -> notExists s fg)
    |> Seq.head

let getEncodedSymbolForDecodedG (allPatterns: string seq): EncodedSymbol =
    
    let fg = getEncodedSymbolsForFandG allPatterns

    // the symbols for one has length 2. One of them is "c" and the other is "f".
    // So whichever symbol is in fg but not in one must therefore be "g"
    
    let symbolsForDigitOne = symbolsForDigitOne allPatterns
    
    fg
    |> Seq.filter (fun s -> notExists s symbolsForDigitOne)
    |> Seq.head

let getEncodedSymbolForDecodedF (allPatterns: string seq): EncodedSymbol =
    let encodedC = getEncodedSymbolForDecodedC allPatterns
    // one contains "c" and "f". 
    // if we know "c", the other symbol in one must be "f"

    symbolsForDigitOne allPatterns
    |> Seq.filter (fun s -> s <> encodedC)
    |> Seq.head

let getEncodedSymbolForDecodedE (allPatterns: string seq): EncodedSymbol =
    
    let encodedA = getEncodedSymbolForDecodedA allPatterns
    let encodedB = getEncodedSymbolForDecodedB allPatterns
    let encodedC = getEncodedSymbolForDecodedD allPatterns
    let encodedD = getEncodedSymbolForDecodedC allPatterns
    let encodedF = getEncodedSymbolForDecodedF allPatterns
    let encodedG = getEncodedSymbolForDecodedG allPatterns

    // if we know everything but "e" then the last remaining symbol must be "e". 
    let symbolsForDigitEight = symbolsForDigitEight allPatterns
    symbolsForDigitEight
    |> Seq.except [ encodedA; encodedB; encodedC; encodedD; encodedF; encodedG ]
    |> Seq.head

let makeDecoder (allPatterns: string seq) =
    let decoder =
        Map.empty
        |> Map.add (getEncodedSymbolForDecodedA allPatterns) "a"
        |> Map.add (getEncodedSymbolForDecodedB allPatterns) "b"
        |> Map.add (getEncodedSymbolForDecodedC allPatterns) "c"
        |> Map.add (getEncodedSymbolForDecodedD allPatterns) "d"
        |> Map.add (getEncodedSymbolForDecodedE allPatterns) "e"
        |> Map.add (getEncodedSymbolForDecodedF allPatterns) "f"
        |> Map.add (getEncodedSymbolForDecodedG allPatterns) "g"
    
    (fun encodedString ->
        encodedString
        |> Helpers.charStrings
        |> Seq.map (fun e -> decoder |> Map.find e)
        |> Seq.sort
        |> String.concat "")
    
let decodePatternToDigit s =
    match (sortLettersInString s) with
    | "abcefg" -> 0
    | "cf" -> 1
    | "acdeg" -> 2
    | "acdfg" -> 3
    | "bcdf" -> 4
    | "abdfg" -> 5
    | "abdefg" -> 6
    | "acf" -> 7
    | "abcdefg" -> 8
    | "abcdfg" -> 9
    | _ -> raise (System.InvalidOperationException($"Could not decode %s{s}"))
    

let decodeScreenToInt screen =
        screen
        |> Seq.map decodePatternToDigit
        |> Seq.map (sprintf "%d")
        |> String.concat ""
        |> int

    
let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun line ->
        let splits = line |> Helpers.splitBy "|" |> Array.map Helpers.trimString
        let splits = splits
                     |> Array.map Helpers.split

        let splits = splits
                     |> Array.map (fun strings ->
                         strings 
                         |> Array.map Helpers.trimString
                         |> Array.map Helpers.stringToUpper
                         |> Array.map sortLettersInString)
                     

        { Inputs = splits[0]; Outputs = splits[1] })
    |> Array.toSeq


let part1 path =
    let inputs = readInput path
    let uniqueDigitLengths = [2; 4; 3; 7]
    
    let uniqueOutputCount =
        inputs
        |> Seq.collect (fun x -> x.Outputs)
        |> Seq.filter (fun x -> (List.contains (x.Length) uniqueDigitLengths))
        |> Seq.length

    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Count of unique outputs: %d{uniqueOutputCount}"
    printfn  " └────────────"

let part2 path =
    let inputs = readInput path
    
    let decodedData =
        inputs
        |> Seq.map (fun data ->
            let allPatterns = Seq.concat [data.Inputs; data.Outputs]
            let decoder = makeDecoder allPatterns
            let outputDigits = data.Outputs |> Seq.map decoder
            (data, outputDigits))
        
    let decodedOutputs =
        decodedData
        |> Seq.map (snd >> decodeScreenToInt)
    
    let decodedSum = Seq.sum decodedOutputs
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Output sum: %d{decodedSum}"
    printfn  " └────────────"


part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"