open CommonHelpers

type StringPair = { Left: string; Right: string }
type Form = { FirstCharacter: string; Counts: Map<StringPair, int64> }
type Rule = { From: StringPair; To: string }
type InputRecord = { Form: Form; Rules: seq<Rule>  }

let leftOfPair p = p.Left
let rightOfPair p = p.Right

let readInput path =
    let chunks = System.IO.File.ReadAllLines(path) |> Helpers.chunkBySeparator Helpers.stringIsEmpty |> Seq.toArray
    
    let initialFormPairs =
        chunks[0]
        |> Seq.item 0 
        |> Helpers.charStrings 
        |> Helpers.seqAsSlidingWindow 2
        |> Seq.map (fun s -> { Left = (s |> Seq.item 0); Right = (s |> Seq.item 1) } )
    
    let counts =
        initialFormPairs
        |> Seq.groupBy id
        |> Seq.map (fun t ->
            let k, s = t
            (k, int64 (Seq.length s)))
        |> Map.ofSeq
        
    let firstCharacter = initialFormPairs |> Seq.item 0 |> leftOfPair
    
    let rules =
        chunks[1]
        |> Seq.map (fun line ->
            let m = System.Text.RegularExpressions.Regex.Match(line, "^(.)(.) -> (.)")
            
            { From = { Left = m.Groups[1].Value; Right = m.Groups[2].Value }; To = m.Groups[3].Value })
        |> Seq.toList

    { Form = { FirstCharacter = firstCharacter; Counts = counts }; Rules = rules }
    
let safeMapLookup k d = d |> Map.tryFind k |> Option.defaultValue (int64 0)

let safeMapIncrement k (v: int64) d =
        d
        |> Map.change k (fun found ->
            match found with
            | Some old -> Some (old + v)
            | None -> Some (v))
        
let processStep (currentForm: Form) (rules: Rule seq) =


    let output =
        rules
        |> Seq.filter (fun rule -> (currentForm.Counts |> safeMapLookup rule.From) > 0)
        |> Seq.fold (fun output rule ->
            let pairsInCurrentForm = currentForm.Counts |> safeMapLookup rule.From
            
            let leftNewPair = { Left = rule.From.Left; Right = rule.To }
            let rightNewPair = { Left = rule.To; Right = rule.From.Right }
            
            output
            |> safeMapIncrement rule.From (-pairsInCurrentForm)
            |> safeMapIncrement leftNewPair pairsInCurrentForm
            |> safeMapIncrement rightNewPair pairsInCurrentForm) currentForm.Counts
        
    { FirstCharacter = currentForm.FirstCharacter; Counts = output }

let buildFrequencyTable (form: Form) =
    form.Counts
    |> Map.toSeq
    |> Seq.fold (fun table n ->
        let pair, count = n
        
        table
        |> safeMapIncrement pair.Right count) Map.empty
    |> safeMapIncrement form.FirstCharacter 1

let runProgram path stepCount =
    let input = readInput path
    let processed = [1..stepCount] |> Seq.fold (fun s _ -> processStep s input.Rules) input.Form
    
    let frequencyTable = buildFrequencyTable processed
    let processedLength = frequencyTable |> Map.toSeq |> Seq.sumBy snd
    
    let mostCommon =
        frequencyTable
        |> Map.fold (fun s k v ->
            if v > (fst s) then (v, k) else s ) ((int64 0), "")
        
    let leastCommon =
        frequencyTable
        |> Map.fold (fun s k v ->
            if v < (fst s) then (v, k) else s ) (System.Int64.MaxValue, "")

    (processedLength, mostCommon, leastCommon)
    

let part1 path =
    let processedLength, mostCommon, leastCommon = runProgram path 10
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Processed length: %d{ processedLength }"
    printfn $" │ Most common: (%s{(snd mostCommon)}, %d{(fst mostCommon)})"
    printfn $" │ Least common: (%s{(snd leastCommon)}, %d{(fst leastCommon)})"
    printfn $" │ Answer: %d{ (fst mostCommon) - (fst leastCommon) }"
    printfn  " └────────────"

let part2 path =
    let processedLength, mostCommon, leastCommon = runProgram path 40
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Processed length: %d{ processedLength }"
    printfn $" │ Most common: (%s{(snd mostCommon)}, %d{(fst mostCommon)})"
    printfn $" │ Least common: (%s{(snd leastCommon)}, %d{(fst leastCommon)})"
    printfn $" │ Answer: %d{ (fst mostCommon) - (fst leastCommon) }"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"