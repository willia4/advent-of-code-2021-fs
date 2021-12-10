
type Position = { Horizontal : int; Depth: int; Aim: int }

let rec makeCommandPart1 (cmd: string) (value: int) =
    match cmd with
    | "down" -> (fun (p: Position) -> {p with Depth = p.Depth + value})
    | "up" -> (fun (p: Position) -> {p with Depth = p.Depth - value})
    | "forward" -> (fun (p: Position) -> {p with Horizontal = p.Horizontal + value})
    | _ -> raise (System.InvalidOperationException($"Could not parse command %s{cmd}"))

let rec makeCommandPart2 (cmd: string) (value: int) =
    match cmd with
    | "down" -> (fun (p: Position) -> {p with Aim = p.Aim + value})
    | "up" -> (fun (p: Position) -> {p with Aim = p.Aim - value})
    | "forward" -> (fun (p: Position) -> {p with Horizontal = p.Horizontal + value; Depth = p.Depth + (p.Aim * value)})
    | _ -> raise (System.InvalidOperationException($"Could not parse command %s{cmd}"))
    
let parseInputLine makeCommand line=
    let regex = new System.Text.RegularExpressions.Regex("^(?<cmd>\w+?) (?<value>\d+?)$")
    let m = regex.Match(line)
    if m.Success then
        let cmd = m.Groups.["cmd"].Value
        let value = m.Groups["value"].Value |> CommonHelpers.Helpers.safeParseInt
        makeCommand cmd value
    else
        raise (System.InvalidOperationException($"Could not parse line %s{line}"))
        

let part1 path =
    let commands = path
                   |> System.IO.File.ReadAllLines
                   |> Array.map (parseInputLine makeCommandPart1)
    
    let finalPosition = Array.fold (fun p cmd -> cmd p) { Horizontal = 0; Depth = 0; Aim = 0 } commands
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Final Horizontal: %d{ finalPosition.Horizontal }"
    printfn $" │ Final Depth: %d{ finalPosition.Depth }"
    printfn $" │ Final Multiple: %d{ finalPosition.Horizontal * finalPosition.Depth }"
    printfn  " └────────────"   

let part2 path =
    let commands = path
                   |> System.IO.File.ReadAllLines
                   |> Array.map (parseInputLine makeCommandPart2)
    
    let finalPosition = Array.fold (fun p cmd -> cmd p) { Horizontal = 0; Depth = 0; Aim = 0 } commands
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Final Horizontal: %d{ finalPosition.Horizontal }"
    printfn $" │ Final Depth: %d{ finalPosition.Depth }"
    printfn $" │ Final Multiple: %d{ finalPosition.Horizontal * finalPosition.Depth }"
    printfn  " └────────────"   

part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"