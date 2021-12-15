module CommonHelpers.Matrix

open System.Collections.Immutable

type Matrix<'a> = ImmutableArray<ImmutableArray<'a>>

let seqToImmutableArray (s: 'a seq) = ImmutableArray.Empty.AddRange(s)

let fromArray (a: 'a[][]): Matrix<'a> =
        a
        |> Array.map seqToImmutableArray
        |> seqToImmutableArray

let fromSeq (s: 'a seq seq): Matrix<'a> =
    s
    |> Seq.map seqToImmutableArray
    |> seqToImmutableArray

let toSeq (matrix: Matrix<'a>) = seq {
    for row in matrix do
        for value in row do
            yield value
}

let toArray (matrix: Matrix<'a>) =
    let rowCount = matrix.Length
    
    let rows = Array.zeroCreate rowCount
    
    for i in [0..(rowCount - 1)] do
        let r = matrix[i]
        rows[i] <- matrix[i] |> Seq.toArray
        
    rows

let fillMatrix width height value: Matrix<'a> =
    [0..height]
    |> List.map (fun _ -> [0..width] |> List.map (fun _ -> value) |> List.toSeq) |> List.toSeq
    |> fromSeq
    
let takeRows count (matrix: Matrix<'a>): Matrix<'a> =
    let rows = Seq.take count matrix
    ImmutableArray.Empty.AddRange(rows)
    
let skipRows count (matrix: Matrix<'a>): Matrix<'a> =
    let rows = Seq.skip count matrix
    ImmutableArray.Empty.AddRange(rows)

let toSeqWithCoords (matrix: Matrix<'a>) = seq {
    let rowCount = matrix.Length
    let colCount = if matrix.Length > 0 then matrix[0].Length else 0
    
    for y in [0..(rowCount - 1)] do
        for x in [0..(colCount - 1)] do
            yield (x, y, matrix[y][x])
}

let map fn (matrix: Matrix<'a>) =
    matrix
    |> Seq.map (fun row -> row |> Seq.map fn |> seqToImmutableArray)
    |> seqToImmutableArray
    
let mapWithCoords fn (matrix: Matrix<'a>) =
    let rowCount = matrix.Length
    let colCount = if matrix.Length > 0 then matrix[0].Length else 0
    
    let mapped = Array.zeroCreate rowCount

    for y in [0..(rowCount - 1)] do
        mapped[y] <- Array.zeroCreate colCount
        for x in [0..(colCount - 1)] do
            mapped[y][x] <- fn x y (matrix[y][x])
    
    mapped |> fromArray

let toString (sep: string) (format: 'a -> string) (matrix: Matrix<'a>) =
    let sb = System.Text.StringBuilder()
    
    let rowString row =
        String.concat sep (row |> Seq.map format)
    
    for row in matrix do
        sb.AppendLine(rowString row) |> ignore

    sb.ToString()
    
let height (matrix: Matrix<'a>) = matrix.Length

let width (matrix: Matrix<'a>) = matrix[0].Length

let columns (matrix: Matrix<'a>): seq<seq<'a>> =
    let column n (matrix: Matrix<'a>) = seq {
        for y in [0..(matrix.Length - 1)] do
            yield matrix[y][n]
    }
    
    seq {
        let colCount = matrix[0].Length
        for x in [0..(colCount - 1)] do
            yield column x matrix
    }

let transpose (matrix: Matrix<'a>): Matrix<'a> =
   matrix |> columns |> fromSeq