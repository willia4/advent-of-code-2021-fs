open CommonHelpers
    
type Symbol =
    | ParenOpen
    | ParenClose
    | SquareBracketOpen
    | SquareBracketClose
    | BraceOpen
    | BraceClose
    | AngleBracketOpen
    | AngleBracketClose

type ParseResult =
    | Corrupt of firstInvalidSymbol: Symbol
    | Incomplete of missingSymbols: Symbol seq
    | Success
    
let isClosingSymbol symbol =
    match symbol with
    | ParenClose -> true
    | SquareBracketClose -> true
    | BraceClose -> true
    | AngleBracketClose -> true
    | _ -> false

let matchingSymbol symbol =
    match symbol with
    | ParenOpen -> ParenClose
    | ParenClose -> ParenOpen
    | SquareBracketOpen -> SquareBracketClose
    | SquareBracketClose -> SquareBracketOpen
    | BraceOpen -> BraceClose
    | BraceClose -> BraceOpen
    | AngleBracketOpen -> AngleBracketClose
    | AngleBracketClose -> AngleBracketOpen

let toSymbol s =
    match s with
    | "(" -> ParenOpen
    | ")" -> ParenClose
    | "[" -> SquareBracketOpen
    | "]" -> SquareBracketClose
    | "{" -> BraceOpen
    | "}" -> BraceClose
    | "<" -> AngleBracketOpen
    | ">" -> AngleBracketClose
    | _ -> raise (System.InvalidOperationException($"Could not understand symbol %s{s}"))

let readInput path =
     System.IO.File.ReadAllLines(path)
     |> Array.map Helpers.charStrings
     |> Array.map (Seq.map toSymbol)
     |> Array.toSeq

let scoreResult res =
    match res with
    | Corrupt invalidChar ->
        match invalidChar with
        | ParenClose -> bigint 3
        | SquareBracketClose -> bigint 57
        | BraceClose -> bigint 1197
        | AngleBracketClose -> bigint 25137
        | _ -> bigint 0
    | Incomplete symbols ->
        symbols |> Seq.fold (fun score s ->
            (score * (bigint 5)) +
                match (matchingSymbol s) with
                | ParenClose -> bigint 1
                | SquareBracketClose -> bigint 2
                | BraceClose -> bigint 3
                | AngleBracketClose -> bigint 4
                | _ -> bigint 0) (bigint 0)
    | _ -> bigint 0

let parse input =
    let rec inner stack input =
        match input with
        | s :: rest -> 
            match isClosingSymbol s with
            | false -> inner (s :: stack) rest
            | true -> match stack with
                      | stackHead :: stackTail ->
                          if (matchingSymbol stackHead) = s then
                              inner (stackTail) rest
                          else
                               Corrupt s
                      | _ -> Incomplete stack
        | _ ->
            if (List.isEmpty stack)
            then Success
            else Incomplete stack
        
    inner [] (input |> Seq.toList)

let part1 path =
    let input = readInput path
    let corrupt = input
                  |> Seq.map parse
                  |> Seq.filter (fun r -> match r with | Corrupt _ -> true | _ -> false)
                  
    let score = corrupt |> Seq.map scoreResult |> Seq.sum
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Corrupt results: %d{Seq.length corrupt}"
    printfn $" │ Score: %A{score}"
    printfn  " └────────────"

let part2 path =
    let input = readInput path
    
    let incomplete = input
                  |> Seq.map parse
                  |> Seq.filter (fun r -> match r with | Incomplete _ -> true | _ -> false)

    let scores = incomplete |> Seq.map scoreResult |> Seq.sort
    let midScore = ((Seq.length scores) - 1) / 2
    let midScore = Seq.item midScore scores

    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Incomplete results: %d{Seq.length incomplete}"
    printfn $" │ Score: %A{midScore}"
    printfn  " └────────────"
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"