open System.Text.RegularExpressions
open CommonHelpers

type Point = { x: int; y: int }
type LineSegment = Point * Point

let readData path: LineSegment array =
    let parseLine line: LineSegment =
        let m = Regex.Match(line, "^(?<x1>\d+?),(?<y1>\d+?) -> (?<x2>\d+?),(?<y2>\d+?)$")
        match m.Success with
        | true -> ( { x = m.Groups["x1"].Value |> Helpers.safeParseInt
                      y = m.Groups["y1"].Value |> Helpers.safeParseInt },
                    { x = m.Groups["x2"].Value |> Helpers.safeParseInt
                      y = m.Groups["y2"].Value |> Helpers.safeParseInt })
        | _ -> raise (new System.InvalidOperationException($"Could not parse %s{line}"))
            
    System.IO.File.ReadAllLines(path)
    |> Array.map parseLine

let points (line: LineSegment) =
    let (p1, p2) = line
    let rec inner acc next =
        let acc = List.append acc [next]
        if next = p2 then acc else
            let xDir = if next.x > p2.x then -1 else if next.x < p2.x then 1 else 0
            let yDir = if next.y > p2.y then -1 else if next.y < p2.y then 1 else 0
            
            inner acc { x = next.x + xDir; y = next.y + yDir }
    
    inner [] p1

let isDiagonal line =
    let (p1, p2) = line
    p1.x <> p2.x && p1.y <> p2.y
    
let isHorizontalOrVertical = isDiagonal >> not

let part1 path =
    let data = readData path
    let data = data |> Array.filter isHorizontalOrVertical

    let allPoints = data |> Array.collect (points >> List.toArray)
    let g = allPoints
            |> Array.groupBy id
            |> Array.filter (fun x -> (x |> snd |> Array.length) >= 2)
            |> Array.map fst
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Overlapping points: %d{ Array.length g }"
    printfn  " └────────────"
    
let part2 path =
    let data = readData path
    let data = data

    let allPoints = data |> Array.collect (points >> List.toArray)
    let g = allPoints
            |> Array.groupBy id
            |> Array.filter (fun x -> (x |> snd |> Array.length) >= 2)
            |> Array.map fst
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Overlapping points: %d{ Array.length g }"
    printfn  " └────────────"  

part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"
