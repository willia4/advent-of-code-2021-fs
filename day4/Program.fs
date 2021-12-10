open System.Runtime.CompilerServices
open CommonHelpers

type BingoBoard = List<List<int * bool>>

let EmptyBoard: BingoBoard = List.empty

let consecutiveSequences (items: seq<'a>): seq<seq<'a>> = seq {
    let buffer = ResizeArray<'a>()
    
    for x in items do
        buffer.Add(x)
        yield buffer
}

let chunkCustom (isSeparator: 'a -> bool) (items: seq<'a>)= 
    let reducer acc next =
        let (chunks, currentChunk) = acc
        let startNewChunk = (isSeparator next) 
        let chunks = if startNewChunk then
                        if (currentChunk |> List.isEmpty |> not) then
                            List.append chunks [currentChunk] 
                        else
                            chunks
                     else
                        chunks
            
        let currentChunk = if startNewChunk then [] else List.append currentChunk [next]

        (chunks, currentChunk)
    
    let (chunks, lastChunk) = items |> Seq.fold reducer ([], [])
    let chunks = if not (List.isEmpty lastChunk) then List.append chunks [lastChunk] else chunks
    
    chunks |> List.map List.toSeq |> List.toSeq
    
let parseBoardRow (line: string) =
    line.Split()
    |> Array.filter (fun x -> not (System.String.IsNullOrWhiteSpace(x)))
    |> Array.map Helpers.safeParseInt
    |> Array.map (fun x -> (x, false))
    |> Array.toList

let parseBoard (boardRows: seq<string>): BingoBoard = boardRows |> Seq.map parseBoardRow |> Seq.toList

let ReadInput path =
    let lines = System.IO.File.ReadAllLines(path) |> Array.toSeq
    let chunks = lines |> Seq.skip 1  |> chunkCustom (System.String.IsNullOrWhiteSpace)
    
    let parseChunk (chunk: seq<string>): BingoBoard = (chunk |> Seq.map parseBoardRow) |> Seq.toList
    
    let boards = chunks
                 |> Seq.map parseChunk
                 |> Seq.toList
    
    let calls = (lines |> Seq.head).Split(",") |> Array.map Helpers.safeParseInt |> Array.toList 
    (calls, boards)

let mark (calls: int seq) (board: BingoBoard): BingoBoard =
    let isCalled value = calls |> Seq.exists (fun x -> x = value)
    
    let markRow row = row
                      |> List.map (fun row ->
                            let (value, marked) = row
                            (value, marked || (isCalled value)))

    board |> List.fold (fun newBoard boardRow -> List.append newBoard [(markRow boardRow)] ) EmptyBoard

let play (calls: int seq) (board: BingoBoard): BingoBoard =
    let (_, board) = calls |> Seq.fold (fun acc nextCall ->
        let (previousCalls, b) = acc
        let previousCalls = List.append previousCalls [nextCall]
        let newBoard = mark previousCalls b
        (previousCalls, newBoard)) ([], board)
    
    board
    
let isWinner (board: BingoBoard) (calls: int seq) =
    let allMarked row = row |> List.forall (fun item ->
        let (_, marked) = item
        marked)
    
    let playedBoard = play calls board
    
    playedBoard |> List.exists allMarked || (playedBoard |> List.transpose |> List.exists allMarked)

let findWinningCall (allCalls: seq<int>) (board: BingoBoard) =
    allCalls
    |> consecutiveSequences
    |> Seq.find (isWinner board)

let scoreBoard (board: BingoBoard) =
    board |> List.collect id |> List.toSeq |> Seq.filter (fun (_, m) -> not m) |> Seq.sumBy (fun (v, _) -> v)
    
let part1 path =
    let (calls, boards) = ReadInput path
    let winners = boards |> List.toSeq |> Seq.map (fun b -> (b, findWinningCall calls b))

    let (firstWinningBoard, firstWinningCalls) = winners |> Seq.sortBy (fun ((_, winningCalls)) -> Seq.length winningCalls) |> Seq.head
    
    // the firstWinningBoard is not marked so we need to play it again to make it marked in order to score it
    let firstWinningBoard = play firstWinningCalls firstWinningBoard
    
    let score = (scoreBoard firstWinningBoard) * (firstWinningCalls |> Seq.last)
    
    printfn  " │ "
    printfn  " │ Part 1: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Winning call: %d{ (firstWinningCalls |> Seq.last) }"
    printfn $" │ Score: %d{ score }"
    printfn  " └────────────"  

let part2 path =
    let (calls, boards) = ReadInput path
    let winners = boards |> List.toSeq |> Seq.map (fun b -> (b, findWinningCall calls b))

    let (lastWinningBoard, lastWinningCalls) = winners |> Seq.sortByDescending (fun ((_, winningCalls)) -> Seq.length winningCalls) |> Seq.head
    
    // the lastWinningBoard is not marked so we need to play it again to make it marked in order to score it
    let lastWinningBoard = play lastWinningCalls lastWinningBoard
    
    let score = (scoreBoard lastWinningBoard) * (lastWinningCalls |> Seq.last)
    
    printfn  " │ "
    printfn  " │ Part 2: "
    printfn $" │ %s{path}"
    printfn  " │ "
    printfn $" │ Winning call: %d{ (lastWinningCalls |> Seq.last) }"
    printfn $" │ Score: %d{ score }"
    printfn  " └────────────"
    
part1 "test_input.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "input.txt"