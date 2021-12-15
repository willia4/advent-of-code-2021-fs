open System.Collections.Generic
open System.Collections.Immutable
open CommonHelpers

let buildMatrix (points: seq<int*int>) =
    let pointsSet = new HashSet<(int*int)>(points)
    
    let minX = 0
    let minY = 0
    let maxX = points |> Seq.maxBy fst |> fst
    let maxY = points |> Seq.maxBy snd |> snd
    
    [0..maxY]
    |> Seq.map (fun y -> [0..maxX] |> Seq.map (fun x -> pointsSet.Contains((x, y))))
    |> Matrix.fromSeq
    

let overlayMatrixes (a: Matrix.Matrix<bool>) (b: Matrix.Matrix<bool>): Matrix.Matrix<bool> =
    let overlayRows rows =
        let (a: bool seq), (b: bool seq) = rows
        Seq.zip a b
        |> Seq.map (fun pts -> (fst pts) || (snd pts))

    Seq.zip a b
    |> Seq.map overlayRows
    |> Matrix.fromSeq
    
let buildVerticalFolder y: (Matrix.Matrix<bool> -> Matrix.Matrix<bool>) =
    (fun matrix ->
        if y <= 1 || y>= matrix.Length then matrix else
            let top = matrix |> Matrix.takeRows y
            let bottom = matrix |> Matrix.skipRows (y + 1)
            
            let flippedBottom: Matrix.Matrix<bool> = ImmutableArray.Empty.AddRange(bottom |> Matrix.skipRows 0 |> Seq.rev)
            
            overlayMatrixes top flippedBottom)

let buildHorizontalFolder x: (Matrix.Matrix<bool> -> Matrix.Matrix<bool>) =
    (fun matrix ->
        let transposed = matrix |> Matrix.transpose
        let folder = buildVerticalFolder x
        
        folder transposed |> Matrix.transpose
    )

let readInput path = 
    let chunks = System.IO.File.ReadAllLines(path) |> Helpers.chunkBySeparator Helpers.stringIsEmpty |> Seq.toArray
    let points = chunks[0] |> Seq.map (fun line ->
        let coords = line.Split(",")
        ((int coords[0]), (int coords[1])))
    
    let matrix = buildMatrix points
    
    let folders =
        chunks[1]
        |> Seq.map (fun line ->
            let line = line.Replace("fold along ", "")
            let instructions = line.Split("=")
            let coord = int instructions[1]
            if instructions[0] = "x" then buildHorizontalFolder coord else buildVerticalFolder coord)

    (matrix, folders)
    

let printMatrix (matrix: Matrix.Matrix<bool>): unit =
    let str = matrix |> Matrix.toString "" (fun b -> if b then "█" else " ")
    for s in str.Split("\r\n") do
        printfn $" │ %s{s}"

let part1 path =
    let isTrue = (fun b -> b = true)
    let matrix, folders = readInput path
    let result = matrix |> (folders |> Seq.item 0)
    let pointCount = result |> Matrix.toSeq |> Seq.filter isTrue |> Seq.length
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Total starting points: %d{ matrix |> Matrix.toSeq |> Seq.filter isTrue |> Seq.length }"
    printfn $" │ Total folding instructions: %d{ folders |> Seq.length }"
    printfn $" │ Points after one fold: %d{ pointCount }"
    printfn  " └────────────"

let part2 path =
    let isTrue = (fun b -> b = true)
    let matrix, folders = readInput path
    
    let foldedMatrix = folders |> Seq.fold (fun m f -> f m) matrix
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Total starting points: %d{ matrix |> Matrix.toSeq |> Seq.filter isTrue |> Seq.length }"
    printfn $" │ Total folding instructions: %d{ folders |> Seq.length }"
    printMatrix foldedMatrix
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"