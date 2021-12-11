open CommonHelpers

let readData path =
    let firstLine = System.IO.File.ReadAllLines(path) |> Array.head
    firstLine.Split(",") |> Array.map int

let countItems inputData =
    let rec inner acc remaining =
        if Seq.isEmpty remaining then
            acc
        else
            let (timerValue, fish) = remaining |> Seq.head

            inner (Map.add timerValue ((Seq.length fish) |> bigint) acc) (remaining |> Seq.tail)
    inner Map.empty (inputData |> Seq.groupBy id)

let simulateDay school =
    let get idx = match Map.tryFind idx school with
                  | Some i -> i
                  | _ -> bigint 0 
    
    // any fish at timer zero will beget a new fish
    let newFish = get 0 

    [0..8] |> List.fold (fun acc idx ->
        // per the rules:
        // - any fish that were 0 will reset to 6. Of course, this is in addition to any fish that were at 7
        // - any fish that were just created will start at 8
        // - every other fish just moves down one (so 5 becomes for) 
        let newCount = match idx with
                       | 6 -> (get 7) + newFish
                       | 8 -> newFish
                       | i -> (get (i + 1))
        acc |> Map.add idx newCount) Map.empty
    
let simulate days path =
    let school = path |> readData |> countItems
       
    let rec inner school days =
        if days = 0 then
            school
        else
            inner (simulateDay school) (days - 1)
    
    inner school days


let part1 path =
    let days = 80
    let totalFish = (simulate days path |> Map.values) :> seq<bigint> |> Seq.sum
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Total fish (after %d{days} days): %A{ totalFish }"
    printfn  " └────────────"

let part2 path =
    let days = 256
    let totalFish = (simulate days path |> Map.values) :> seq<bigint> |> Seq.sum
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Total fish (after %d{days} days): %A{ totalFish }"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"