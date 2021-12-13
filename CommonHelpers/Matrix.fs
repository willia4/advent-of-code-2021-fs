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
    