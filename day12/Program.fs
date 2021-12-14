type Graph = (string * string) seq
type Path = string seq

let startNode = "start"
let endNode = "end"

let readInput path: Graph=
    System.IO.File.ReadAllLines(path)
    |> Seq.map (fun line ->
        let nodes = line.Split("-")
        (nodes[0], nodes[1]))
    
let graphNodes (g: Graph) =
    g
    |> Seq.collect (fun e ->
        let a, b = e
        [a; b])
    
let connectionsToNode (g: Graph) n =
    let aConnections = g |> Seq.filter (fun e -> fst e = n) |> Seq.map snd
    let bConnections = g |> Seq.filter (fun e -> snd e = n) |> Seq.map fst
    
    (Seq.append aConnections bConnections) |> Seq.distinct

let pathToString (p: Path) = p |> String.concat "\\"

let findPaths (g: Graph) (mayRevisit: Set<string>) (mayRevisitOnce: Set<string>) =
    let isAllowed (currentPath: Path) currentNode =
        if Seq.contains currentNode currentPath |> not then true // if we haven't visited at all, it's fair game
        else if Set.contains currentNode mayRevisit then true // if we may revisit as many times as we'd like, go for it
        else if Set.contains currentNode mayRevisitOnce |> not then false // if we're not allowed to revisit even once, then fail 
        else (currentPath |> Seq.filter (fun n -> n = currentNode) |> Seq.length) = 1 // we may only revisit once, so find out if we've already visited twice or not
    
    let rec visit (allPaths: Path list) (currentPath: Path) currentNode =
        // we're not allowed to visit this node. Don't add it to the current path.
        // but make sure that the current path is in our total list of paths and return.
        if currentNode |> isAllowed currentPath |> not then
            List.append allPaths [currentPath]
        else
            let currentPath = Seq.append currentPath [currentNode]
            if (currentNode = endNode) then
                allPaths |> List.append [currentPath]
            else
                let branches =
                    connectionsToNode g currentNode
                    |> Seq.collect (fun c -> visit allPaths currentPath c)
                List.append allPaths (branches |> Seq.toList)
            
    let paths = visit [] [] startNode
    paths
    |> Seq.filter (fun p -> (Seq.last p) = endNode)
    |> Seq.distinctBy pathToString

let isUppercase (s: string) = s.ToUpperInvariant() = s
let isLowercase = isUppercase >> not

let part1 path =
    let g = readInput path
    
    let bigNodes = g |> graphNodes |> Seq.filter isUppercase
    let mayRevisit = bigNodes |> Set.ofSeq
    let mayRevisitOnce = Set.empty
    
    let paths = findPaths g mayRevisit mayRevisitOnce

    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Paths from start to end: %d{ paths |> Seq.length}"
    printfn  " └────────────"

let part2 path =
    let g = readInput path
    
    let bigNodes = g |> graphNodes |> Seq.filter isUppercase
    let smallNodes = g |> graphNodes |> Seq.filter isLowercase |> Seq.except [startNode; endNode]

    let mayRevisit = bigNodes |> Set.ofSeq
    
    let paths =
        smallNodes |> Seq.collect (fun singleSmallNode ->
            let mayRevisitOnce = [ singleSmallNode ] |> Set.ofList
            findPaths g mayRevisit mayRevisitOnce)
        |> Seq.distinctBy pathToString

    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Paths from start to end: %d{ paths |> Seq.length}"
    printfn  " └────────────"
    
part1 "test_input1.txt"
part1 "test_input2.txt"
part1 "test_input3.txt"
part1 "input.txt"
    
part2 "test_input1.txt"
part2 "test_input2.txt"
part2 "test_input3.txt"
part2 "input.txt"