open System.Collections.Generic
open CommonHelpers

type TileMap<'a> = Matrix.Matrix<Matrix.Matrix<'a>>

let makeTileRow (width: int) (f: 'a -> 'a) (firstTile: Matrix.Matrix<'a>): Matrix.Matrix<'a>[] =
    let rec inner row remaining =
        if (remaining <= 0) then row
        else
            let newest = row |> List.last
            let newest = newest |> Matrix.map f
            inner (List.append row [ newest ]) (remaining - 1)

    inner [ firstTile ] (width - 1) |> List.toArray

let makeTileMap width height (f: 'a -> 'a) (firstTile: Matrix.Matrix<'a>): TileMap<'a> =
    let rows =
        [1..height]
        |> List.fold (fun rows n ->
            let ft = if List.isEmpty rows then firstTile else (List.last rows |> Array.item 0 |> Matrix.map f)
            List.append rows [ makeTileRow width f ft]) []
        |> List.toArray
    rows |> Matrix.fromArray
    
let tileMapValue x y (tileMap: TileMap<'a>) =
    let firstTile = tileMap[0][0]
    
    let tileHeight = firstTile.Length
    let tileWidth = firstTile[0].Length

    let tileX = x / tileWidth
    let tileY = y / tileHeight
    let offsetX = (x - (tileX * tileWidth))
    let offsetY = (y - (tileY * tileHeight))
    
    (((tileMap[tileY])[tileX])[offsetY])[offsetX]
    
let tileMapToMatrix (tileMap: TileMap<'a>): Matrix.Matrix<'a> =
    let mapHeight = tileMap.Length
    let mapWidth = tileMap[0].Length
    
    let firstTile = tileMap[0][0]
    
    let tileHeight = firstTile.Length
    let tileWidth = firstTile[0].Length
    
    let totalHeight = mapHeight * tileHeight
    let totalWidth = mapWidth * tileWidth
    
    let makeTileRow r: seq<'a> = seq {
        for x in [0..(totalWidth - 1)] do
            yield (tileMapValue x r tileMap) 
    }
    
    seq {
        for r in [0..(totalHeight - 1)] do
            yield makeTileRow r
    } |> Matrix.fromSeq
    
let readInput path =
    System.IO.File.ReadAllLines(path)
    |> Array.map (fun line ->
        line
        |> Helpers.charStrings
        |> Seq.map int
        |> Seq.toArray)
    |> Matrix.fromArray

let neighbors (pt: int*int) (m: Matrix.Matrix<'a>) =
    let isValid pt =
        let x, y = pt
        (y >= 0 && y < m.Length && x >= 0 && x < m[y].Length)

    let x, y = pt
    [(x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1)]
    |> Seq.filter isValid

let findPath (m: Matrix.Matrix<int>) =
    let startNode = (0, 0)
    let endNode = (m[0].Length - 1, m.Length - 1)
    
    let openSet = new PriorityQueue<int*int, int>()
    let openSetHash = new HashSet<int*int>()
    let cameFrom = new Dictionary<int*int, int*int>()
    let gScore = new Dictionary<int*int, int>()
    
    for p in (Matrix.allCoordinates m) do
        gScore[p] <- System.Int32.MaxValue

    openSet.Enqueue(startNode, m[snd startNode][fst startNode])
    openSetHash.Add(startNode) |> ignore
    
    gScore[startNode] <- m[snd startNode][fst startNode]

    let reconstructPath current =
        let rec inner path current =
            if cameFrom.ContainsKey(current) |> not then
                (List.append path [current]) |> List.rev
            else
                inner (List.append path [current]) (cameFrom[current])
            
        inner [ ] current
        
    let rec innerLoop current =
        if current = endNode then
            reconstructPath current
        else
            let neighbors = neighbors current m
            for n in neighbors do
                let tentative_gScore = gScore[current] + m[(snd n)][(fst n)]
                if tentative_gScore < gScore[n] then
                    cameFrom[n] <- current
                    gScore[n] <- tentative_gScore

                    if openSetHash.Contains(n) |> not then
                        openSetHash.Add(n) |> ignore
                        openSet.Enqueue(n, gScore[n])
                    else ()
                else ()
            innerLoop (openSet.Dequeue())
    
    innerLoop (openSet.Dequeue())
                

let printMatrixAndPath (matrix:Matrix.Matrix<int>) (path: (int*int) list) =
    let s =
        matrix
        |> Matrix.toStringWithCoords "" (fun pt v ->
            if (List.exists (fun x -> x = pt) path) then
                $"\x1b[97m{v}\x1b[0m"
            else
                $"\x1b[90m{v}\x1b[0m")
        
    printfn "%s" s

let part1 path =
    let m = readInput path
    let shortestPath = findPath m
    let totalRisk = shortestPath |> Seq.skip 1 |> Seq.sumBy (fun pt -> m[snd pt][fst pt])
    
    if (path.Contains("test_input.txt")) then
        printMatrixAndPath m shortestPath        
    else ()
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Shortest path length: %d{ shortestPath |> Seq.length }"
    printfn $" │ Total Risk: %d{totalRisk}"
    printfn  " └────────────"

let part2 path =
    let input = readInput path
    
    let tileTransform x = if x >= 9 then 1 else (x + 1)
    
    let fullMap =
        input
        |> makeTileMap 5 5 tileTransform
        |> tileMapToMatrix
        
    let shortestPath = findPath fullMap
    let totalRisk = shortestPath |> Seq.skip 1 |> Seq.sumBy (fun pt -> fullMap[snd pt][fst pt])
    
    if (path.Contains("test_input.txt")) then
        printMatrixAndPath fullMap shortestPath        
    else ()

    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Shortest path length: %d{ shortestPath |> Seq.length }"
    printfn $" │ Total Risk: %d{totalRisk}"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"
