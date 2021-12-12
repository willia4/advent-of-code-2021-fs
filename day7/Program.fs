open CommonHelpers

let readInput path =
    System.IO.File.ReadAllText(path)
    |> Helpers.trimString
    |> Helpers.splitBy ","
    |> Array.map Helpers.trimString
    |> Array.map int
    
let fuelCostsForMovingToPosition (calc: int -> int -> int) (desiredPosition: int) (positions: int seq) =
    let calc = calc desiredPosition
    
    positions |> Seq.map calc
    
let minimizeFuelCosts calc (positions: int seq) =
    let max = positions |> Seq.max
    let min = positions |> Seq.min
    
    let costs = ([min..max]) :> seq<int>
                |> Seq.map (fun p ->
                    let cost = positions |> fuelCostsForMovingToPosition calc p |> Seq.sum
                    (p, cost)
                    )
                
    costs |> Seq.sortBy snd |> Seq.head 

let part1 path =
    let input = readInput path
    let costFun desiredPosition startingPosition = abs (desiredPosition - startingPosition)
    
    let (minPos, minCost) = minimizeFuelCosts costFun input
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Winning position: %d{minPos}"
    printfn $" │ Winning fuel cost: %d{minCost}"
    printfn  " └────────────"

let part2 path =
    let input = readInput path
    let costFun desiredPosition startingPosition =
        let delta = abs ((desiredPosition |> double) - (startingPosition |> double))
        ceil (delta * (delta + 1.0)) / 2.0 |> int 

    let (minPos, minCost) = minimizeFuelCosts costFun input
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Winning position: %d{minPos}"
    printfn $" │ Winning fuel cost: %d{minCost}"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"