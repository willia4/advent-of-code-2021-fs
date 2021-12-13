open System.Collections.Immutable
open CommonHelpers
type Matrix<'a> = Matrix.Matrix<'a>

let inc v = v + 1
let matrixToString: Matrix<int> -> string = Matrix.toString "" (fun v ->
    if v = 0 then
        $"\x1b[97m%d{v}\x1b[0m"
    else
        let v = match v with
                | n when n < 10 -> $"%d{n}"
                | 10 -> "A"
                | 11 -> "B"
                | 12 -> "C"
                | _ -> "*"
                
        $"\x1b[90m%s{v}\x1b[0m")

let matrixToString_: Matrix<int> -> string = Matrix.toString "" (fun v ->
    if v = 0 then
        " "
    else
        $"%d{v}")

let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun line -> line |> Helpers.charStrings |> Seq.map int |> Seq.toArray)
    |> Matrix.fromArray
    
let matrixItem p (matrix: Matrix<'a>) =
    let x, y = p
    matrix[y][x]

let neighbors (matrix: Matrix<'a>) p =
    let x, y = p
    let isValid p' =
        let x', y' = p'
        x' >= 0 && x' < matrix[0].Length && y' >= 0 && y' < matrix.Length
    
    [(x - 1, y); (x - 1, y - 1); (x, y - 1); (x + 1, y - 1); (x + 1, y);
     (x + 1, y + 1); (x, y + 1); (x - 1, y + 1)]
    |> List.filter isValid
    |> List.toSeq
    
let step (matrix: Matrix<int>) =
    let matrix = matrix |> Matrix.map inc
    
    let canFlash m = m |> Matrix.toSeq |> Seq.exists (fun v -> v = 10)

    let rec flash acc =
        let m, flashCount = acc

        if not (canFlash m) then (m, flashCount)
        else
            let ready =
                m
                |> Matrix.toSeqWithCoords
                |> Seq.filter (fun xyv ->
                    let _, _, v = xyv
                    v = 10)
                |> Seq.map (fun xyv ->
                    let x, y, _ = xyv
                    (x, y))
                |> Seq.toList

            let readyNeighbors =
                ready
                |> Seq.collect (fun xy ->
                    neighbors m xy)
                |> Seq.toList

            // any that are going to flash shouldn't flash again so set them past 10
            let m =
                m
                |> Matrix.mapWithCoords (fun x y v ->
                    if (ready |> List.contains (x, y)) then 11 else v)
                
            let m =
                m
                |> Matrix.mapWithCoords (fun x y v ->
                    if v > 10 then v else 
                        let flashCount = readyNeighbors
                                         |> List.filter (fun n -> n = (x, y))
                                         |> List.length 
                        min (v + flashCount) 10)
            flash (m, (flashCount + (List.length ready)))

    let matrix, flashCount = flash (matrix, 0)
    let matrix = matrix |> Matrix.map (fun v -> if v > 9 then 0 else v)
    
    (matrix, flashCount)

let isSynchronized (matrix: Matrix<int>) =
    matrix |> Matrix.toSeq |> Seq.forall (fun v -> v = 0)

let part1 path =
    let matrix = readInput path
    let acc = [1..100] |> Seq.fold (fun acc _ ->
        let matrix, flashCount = acc
        let matrix, newFlashCount = step matrix
        (matrix, newFlashCount + flashCount)) (matrix, 0)

    let matrix, flashCount = acc
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Total number of flashes: %d{flashCount}"
    printfn  " └────────────"

let part2 path =
    let matrix = readInput path
    
    let rec inner acc =
        let matrix, stepCount = acc
        if isSynchronized matrix then acc
        else
            let matrix, _ = step matrix
            inner (matrix, stepCount + 1)
        
    let matrix, stepCount = inner (matrix, 0)

    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Synchronized at: %d{stepCount}"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"
