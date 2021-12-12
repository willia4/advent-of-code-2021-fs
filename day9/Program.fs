open CommonHelpers

type IntMatrix = int[][]

let readInput path =
    let digitStringToIntList = Helpers.charStrings >> (Seq.map int) >> Seq.toArray 
    System.IO.File.ReadAllLines(path)
    |> Array.map digitStringToIntList

let matrixValue x y data = data |> Array.item y |> Array.item x

let adjacentPoints x y data =
    let valid pos =
        let (x', y') = pos
        x' >= 0 && y' >= 0 && y' < (Array.length data) && x' < ((Array.item y' data) |> Array.length)
    
    [(x - 1, y)
     (x, y - 1)
     (x + 1, y)
     (x, y + 1)]
    |> Seq.filter valid
    |> Seq.map (fun pos ->
        let (x', y') = pos
        let row = Array.item y' data
        let value = Array.item x' row
        (value, x', y'))

let matrixCoords data =
    [0..((Seq.length data) - 1)]
    |> Seq.collect (fun y ->
        let row = Seq.item y data
        [0..((Seq.length row) - 1)]
        |> Seq.map (fun x -> (x, y))) 
    
let matrixValues data =
    data
    |> matrixCoords
    |> Seq.map (fun c ->
        let x, y = c
        ((matrixValue x y data), x, y))

let findLowPoints data =
    data
    |> matrixValues
    |> Seq.filter (fun vt ->
        let (v, x, y) = vt
        adjacentPoints x y data
        |> Seq.forall (fun adj -> (Helpers.fst3 adj) > v))

let findBasinForLowPoint x y data =
    let basin = System.Collections.Generic.HashSet<int * int>()
    
    let rec inner remaining =
        match remaining with
        | head :: remaining ->
            if (basin.Add(head)) then
                let x, y = head
                let neighbors =
                    adjacentPoints x y data
                    |> Seq.filter (fun adj -> (Helpers.fst3 adj) <> 9)
                    |> Seq.map (fun adj ->
                        let _, adjX, adjY = adj
                        (adjX, adjY))
                    |> Seq.toList
                
                let remaining = (List.distinct (remaining @ neighbors))
                inner remaining
            else inner remaining
        | [] -> ()

    inner [(x, y)]
    
    // force it to be a List to make sure we don't let a mutable object escape this function
    basin :> seq<int * int> |> Seq.toList :> seq<int * int>

let part1 path =
    let data = readInput path
    let lowPoints = findLowPoints data
    let riskLevels = lowPoints |> Seq.map (fun x -> (Helpers.fst3 x) + 1)
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Sum of risk levels %d{riskLevels |> Seq.sum}"
    printfn  " └────────────"

let part2 path =
    let data = readInput path
    
    let lowPoints = findLowPoints data
    let basins =
        lowPoints
        |> Seq.map (fun lp ->
            let (_, x, y) = lp
            findBasinForLowPoint x y data)
    
    let biggestBasins =
        basins
        |> Seq.sortByDescending Seq.length
        |> Seq.take 3
        
    let biggestSizes = biggestBasins |> Seq.map (Seq.length >> bigint)
    
    let productOfSizes = biggestSizes |> Seq.fold (fun p next -> p * next) (bigint 1)
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Found basins: %d{basins |> Seq.length}"
    printfn $" │ Product of sizes of top 3 basins: %A{productOfSizes}"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"
