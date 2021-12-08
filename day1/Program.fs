
let readInput path =
    path
    |> System.IO.File.ReadAllLines
    |> Array.map CommonHelpers.Helpers.safeParseInt
    |> Array.toSeq
    
let countIncreases (chunks: seq<seq<int>>): int =
    let chunks = chunks |> Seq.map (Seq.sum)
    printfn $"%A{chunks}"
    if (chunks |> Seq.length) >= 2 then
        let rec inner prev rest acc =
            if (rest |> Seq.isEmpty |> not) then
               let current = rest |> Seq.head
               inner current (rest |> Seq.skip 1) (if current > prev then acc + 1 else acc)
            else
                acc
        
        inner (chunks |> Seq.head) (chunks |> Seq.skip 1) 0
    else
        0
    
let part1 (path:string) : unit =
    let inputs = path
                 |> readInput
                 |> Seq.chunkBySize 1
                 |> Seq.map (Array.toSeq)
                 |> countIncreases
    
    printfn "%s" "Part 1"
    printfn $"%s{path}"
    printfn $"%d{inputs}"
   
let part2 (path:string) : unit =
    let inputs = path
                 |> readInput
                 |> Seq.chunkBySize 3
                 |> Seq.map (Array.toSeq)
                 |> countIncreases
    
    printfn "%s" "Part 2"
    printfn $"%s{path}"
    printfn $"%d{inputs}" 
    
    
//part1 "test_input.txt"
//part1 "input.txt"

part2 "test_input.txt"
//part2  "input.txt"
