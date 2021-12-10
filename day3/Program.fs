open CommonHelpers

type Bit = Zero | One

let bitToInt b = match b with | Zero -> 0 | One -> 1

let bitsToInt (bits: seq<Bit>) =
    let bits = bits |> Seq.toList |> List.rev |> List.map bitToInt
    let indexes = (seq { 0..(bits.Length - 1) } |> Seq.toList)
    List.zip bits indexes
    |> List.fold (fun acc (nextBit, nextIndex) -> acc + (nextBit <<< nextIndex)) 0

let bitValue i (s: string) =
    match s.Substring(i, 1) with
    | "0" -> Zero
    | "1" -> One
    | _ -> raise (new System.InvalidOperationException($"Could not get %d{i}th digit of %s{s}"))

let stringToBits s = s |> Helpers.charStrings |> Seq.map (bitValue 0)
    
let countBits i (strings: string list) =
    let counts = strings
                 |> List.map (bitValue i)
                 |> Helpers.countOccurrences
    
    let zeros = match Map.tryFind Zero counts with | Some c -> c | None -> 0
    let ones = match Map.tryFind One counts with | Some c -> c | None -> 0
    (zeros, ones)

let mostCommon i prefer strings =
    let (zeros, ones) = countBits i strings
    if zeros = ones then prefer else if ones > zeros then One else Zero  
    
let leastCommon i prefer strings =
    let (zeros, ones) = countBits i strings
    if zeros = ones then prefer else if ones < zeros then One else Zero

let filterStringsToSingleNumber (filterFun: int -> List<string> -> List<string>) (strings: string list) =
    let rec inner index (remaining: string list) =
        if remaining.Length = 1 then
            remaining.Head |> stringToBits
        else
            inner (index + 1) (remaining |> filterFun index)

    inner 0 strings 
    

let readInput path =
    path
    |> System.IO.File.ReadAllLines
    |> Array.toList

let part1 path =
    let input = readInput path
    let wordLength = (input |> List.head).Length
    
    let gamma = seq { 0..(wordLength - 1) }
                  |> Seq.map (fun i -> mostCommon i One input)
                  |> bitsToInt
    
    let epsilon = seq { 0..(wordLength - 1) }
                  |> Seq.map (fun i -> leastCommon i One input)
                  |> bitsToInt

    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Gamma: %d{ gamma }"
    printfn $" │ Epsilon: %d{ epsilon }"
    printfn $" │ Power: %d{ gamma * epsilon }"
    printfn  " └────────────"   
    
let part2 path =
    let input = readInput path
    let wordLength = (input |> List.head).Length
    
    
    let o2 = input
             |> filterStringsToSingleNumber (fun index remaining ->
                    let filter = mostCommon index One remaining
                    remaining |> List.filter (fun x -> bitValue index x = filter))
             |> bitsToInt

    let co2 = input
             |> filterStringsToSingleNumber (fun index remaining ->
                    let filter = leastCommon index Zero remaining
                    remaining |> List.filter (fun x -> bitValue index x = filter))
             |> bitsToInt

    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ O2: %d{o2}"
    printfn $" | CO2: %d{co2}"
    printfn $" | Life Support Rating: %d{o2 * co2}"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"