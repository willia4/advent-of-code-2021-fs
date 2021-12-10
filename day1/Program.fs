
let readInput path =
    path
    |> System.IO.File.ReadAllLines
    |> Array.map CommonHelpers.Helpers.safeParseInt
    |> Array.toSeq
    
let slidingWindows (windowSize: int) (items: seq<'a>) : seq<seq<'a>> = seq{
    let buffer = ResizeArray<'a>(windowSize)
    
    for x in items do
        if buffer.Count = windowSize then
            yield (Seq.toList buffer)
            buffer.RemoveAt(0)
        else ()
        buffer.Add(x)

    if buffer.Count > 0 then yield (Seq.toList buffer) else ()
}
    
    
let countIncreases (chunks: seq<seq<int>>): int =
    let chunks = chunks |> Seq.map (Seq.sum) |> Seq.toList

    if (chunks |> List.length) >= 2 then
        let rec inner prev rest acc =
            match rest with
            | head :: tail -> inner head tail (if head > prev then acc + 1 else acc)
            | _ -> acc
        
        inner (chunks |> List.head) (chunks |> List.tail) 0
    else
        0
    
let part1 (path:string) : unit =
    let increases = path
                 |> readInput
                 |> slidingWindows 1
                 |> countIncreases
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Number of increases: %d{ increases }"
    printfn  " └────────────"   
   
let part2 (path:string) : unit =
    let increases = path
                 |> readInput
                 |> slidingWindows 3
                 |> countIncreases
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Number of increases: %d{ increases }"
    printfn  " └────────────"   
    
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2  "input.txt"
