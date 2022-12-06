//#r "..\\CommonHelpers\\bin\\Debug\\net6.0\\CommonHelpers.dll";;
open CommonHelpers

type Token =
  | OpenBraket
  | CloseBracket
  | SimpleValue of int
  | Comma
  with
  override self.ToString() =
    match self with
    | OpenBraket -> "["
    | CloseBracket -> "]"
    | SimpleValue v -> "v"
    | Comma -> ","

type Tokenizer = {
  data: string
  index: int
}

module Tokenizer =
  let fromString s = { data= s; index = -1 }
  let remainingData t = (t.data.Length - 1) - (t.index)

  let nextChar t =
    let nextIndex = t.index + 1
    if nextIndex < t.data.Length then

      Some (t.data.Substring(nextIndex, 1)), { t with index = nextIndex}
    else
      None, t

  let peekChar = nextChar >> fst

  let hasNextChar = peekChar >> Option.isSome

  let rec eatSpaces t =
    match nextChar t with
    | Some c, nextT when c = " " -> eatSpaces nextT
    | _ -> t

  let remainingString t =
    let idx = t.index + 1
    if t.data.Length > idx then t.data.Substring(idx) else ""

  let private tryOpenBracket t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "[" -> Some (OpenBraket, nextTokenizer)
    | _ -> None

  let private tryCloseBracket t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "]" -> Some (CloseBracket, nextTokenizer)
    | _ -> None

  let private tryComma t =
    let (nextChar, nextTokenizer) = nextChar t
    match nextChar with
    | Some c when c = "," -> Some (Comma, nextTokenizer)
    | _ -> None

  let private tryNumber t =
    let isDigit (s: string) = "0123456789".Contains(s)

    let rec inner (acc: string) t =
      match nextChar t with
      | Some d, nextT when isDigit d -> inner (acc + d) nextT
      | _ -> acc, t

    let buffer, nextT = inner "" t
    if (Helpers.stringNotEmpty buffer) then
      Some (SimpleValue (System.Int32.Parse(buffer)), nextT)
    else
      None

  let nextToken t =
    let t = eatSpaces t

    let possibleToken =
      [ tryOpenBracket t
        tryCloseBracket t
        tryComma t
        tryNumber t ]
      |> List.tryPick id

    match possibleToken with
    | Some (token, nextT) -> Some token, nextT
    | _ -> None, t

  let peekToken t =
    match nextToken t with
    | Some token, _ -> Some token
    | _ -> None

  let hasNextToken = peekToken >> Option.isSome

let panicIf b msg =
  if b then (raise (System.InvalidOperationException msg)) else ()

let panic msg = panicIf true msg

[<ReferenceEquality>]
type SnailNumber =
  | Simple of int
  | Pair of SnailNumber * SnailNumber
  with
  override self.ToString() =
    match self with
    | Simple i -> $"{i}"
    | Pair (left, right) -> $"[{left},{right}]"

module SnailNumber =
  let getValue sn =
    match sn with
    | Simple i -> i
    | _ -> panic $"Could not get value of node {sn}"; 0

  let fromString (s: string) =
    let rec parseSnailNumber t =
      let parseCompoundSnailNumber t =
        let openBracket, t = Tokenizer.nextToken t
        panicIf (openBracket |> Option.isNone) "Did not get any token in the middle of a compound expression"
        panicIf (Option.get openBracket <> Token.OpenBraket) $"Did not get a openBracket in the middle of a compound expression: {Option.get openBracket}"

        let left, t = parseSnailNumber t
        let comma, t = Tokenizer.nextToken t

        panicIf (comma |> Option.isNone) "Did not get any token in the middle of a compound expression"
        panicIf (Option.get comma <> Token.Comma) $"Did not get a comma in the middle of a compound expression: {Option.get comma}"

        let right, t = parseSnailNumber t

        let closeBracket, t = Tokenizer.nextToken t
        panicIf (closeBracket |> Option.isNone) "Did not get any token at the end of a compound expression"
        panicIf (Option.get closeBracket <> Token.CloseBracket) $"Did not get a closeBracket at the end of a compound expression: {Option.get closeBracket}"

        SnailNumber.Pair(left, right), t

      let parseSimpleSnailNumber t =
        match Tokenizer.nextToken t with
        | Some (Token.SimpleValue v), nextT -> SnailNumber.Simple v, nextT
        | Some otherToken, _ -> panic $"Unexpected simple value token {otherToken}"; SnailNumber.Simple 0, t
        | None, _ -> panic $"Unexpected None for a simple value token"; SnailNumber.Simple 0, t

      match Tokenizer.peekToken t with
      | Some OpenBraket -> parseCompoundSnailNumber t
      | Some (SimpleValue _) -> parseSimpleSnailNumber t
      | Some otherToken -> panic $"Unexpected token at start of expression {otherToken}"; SnailNumber.Simple 0, t
      | None -> panic $"Unexpected None at start of expression"; SnailNumber.Simple 0, t

    parseSnailNumber (Tokenizer.fromString s) |> fst

  let tryDepth (root: SnailNumber) (s: SnailNumber) =
    let rec inner depth (root: SnailNumber) (s: SnailNumber) =
      if root = s
        then Some depth
        else
          match root with
          | Simple _ -> None
          | Pair (left, right) ->
              [inner (depth + 1) left s
               inner (depth + 1) right s]
              |> List.tryPick id
    inner 0 root s

  let tryFindParent (root: SnailNumber) (s: SnailNumber) =
    let rec inner root s =
      match root with
      | Simple _ -> None
      | Pair (left, right) ->
        if left = s || right = s then Some root
        else
          [ inner left s
            inner right s ]
          |> List.tryPick id

    if (s = root) then None
    else inner root s

  let tryGetLeft sn =
    match sn with
    | Pair ( left, _) -> Some left
    | _ -> None

  let tryGetRight sn =
    match sn with
    | Pair ( _, right) -> Some right
    | _ -> None

  let isSimplePair sn =
    match sn with
    | Pair (Simple _, Simple _) -> true
    | _ -> false

  let isSimple sn =
    match sn with
    | Simple _ -> true
    | _ -> false

  let tryNavigate (directions: string) sn =
    let rec inner sn (directions: string) =
      if System.String.IsNullOrWhiteSpace(directions) then sn
      else
        match sn with
        | Some sn ->
            let firstCharacter = directions.Substring(0, 1)
            let rest = directions.Substring(1)

            match sn with
            | Simple _ -> None
            | Pair (left, right) ->
              if (firstCharacter = "l" || firstCharacter = "L")
                then inner (Some left) rest
                elif (firstCharacter = "r" || firstCharacter = "R")
                  then inner (Some right) rest
                else None
        | None -> None

    inner (Some sn) directions


  // I'm not sure what "the first regular number to the [left|right] of the pair manes when translated to binary tree operations"
  // but it seems to be that if I just order them in the same way they were written as a string, it's literally just "first to the left" or "first to the right"
  let rec orderNodesLexically sn =
    match sn with
    | Simple _ -> [sn]
    | Pair (left, right) -> List.append (orderNodesLexically left) (orderNodesLexically right)

  let rec tryFindLeftMostSimpleNumber (pred) sn =
    let lexical = orderNodesLexically sn
    lexical |> List.tryFind pred

  let tryFindSimpleNumberToTheLeft root sn =
    let lexical = orderNodesLexically root
    let i_sn = lexical |> List.findIndex (fun n -> n = sn)
    if i_sn > 0 then
      lexical |> List.item (i_sn - 1) |> Some
    else None

  let tryFindSimpleNumberToTheRight root sn =
    let lexical = orderNodesLexically root
    let i_sn = lexical |> List.findIndex (fun n -> n = sn)
    if i_sn < ((List.length lexical) - 1) then
      lexical |> List.item (i_sn + 1) |> Some
    else None

  let rec replaceNode root oldNode newNode =
    let oldParent = tryFindParent root oldNode
    let newParent =
      match oldParent with
      | None -> newNode
      | Some oldParent ->
        match oldParent with
        | Pair (left, right ) ->
          if left = oldNode then
            Pair (newNode, right)
          else
            Pair (left, newNode)
        | _ -> panic "Parent node was somehow not a pair"; root

    match oldParent with
    | None -> newNode
    | Some oldParent -> replaceNode root oldParent newParent

  let tryFindNeedsExploding sn =
    let findDepth = tryDepth sn
    let isPartOfSimplePair sn' =
      if (isSimple sn') then
        match tryFindParent sn sn' with
        | Some parent -> isSimplePair parent
        | _ -> false
      else false

    tryFindLeftMostSimpleNumber (fun sn ->
      if (isPartOfSimplePair sn) then
        match findDepth sn with
        | Some depth -> depth >= 5
        | _ -> false
      else false) sn
    |> Option.bind (tryFindParent sn)

  let explodePair root sn =
    match sn with
    | Pair (left, right) ->
      let leftSibling = tryFindSimpleNumberToTheLeft root left
      let rightSibling = tryFindSimpleNumberToTheRight root right

      let root =
        match leftSibling with
        | Some leftSibling -> replaceNode root leftSibling (Simple ((getValue leftSibling) + (getValue left)))
        | _ -> root

      let root =
        match rightSibling with
        | Some rightSibling -> replaceNode root rightSibling (Simple ((getValue rightSibling) + (getValue right)))
        | _ -> root

      replaceNode root sn (Simple 0)
    | _ -> panic $"Given a non-pair element to explode {sn}"; root

  let explodeIfNecessary root =
    let shouldExplode = tryFindNeedsExploding root
    match shouldExplode with
    | Some shouldExplode ->
      true, (explodePair root shouldExplode)
    | _ -> false, root

  let tryFindNeedsSplitting root =
    root
    |> orderNodesLexically
    |> List.tryFind (function
                     | Simple i when i >= 10 -> true
                     | _ -> false)

  let splitNode root sn =
    match sn with
    | Simple v ->
      let newLeft = ((float v) / 2.0) |> floor |> int
      let newRight = ((float v) / 2.0) |> ceil |> int

      replaceNode root sn (Pair ((Simple newLeft), (Simple newRight)))
    | _ -> panic "Could not split non simple node"; root

  let splitNodeIfNecessary root =
    let shouldSplit = tryFindNeedsSplitting root
    match shouldSplit with
    | Some shouldSplit -> true, (splitNode root shouldSplit)
    | _ -> false, root

  let rec reduce sn =
    match explodeIfNecessary sn with
    | true, exploded -> reduce exploded
    | false, _ ->
      match splitNodeIfNecessary sn with
      | true, split -> reduce split
      | false, _ -> sn

  let add (a: SnailNumber) (b: SnailNumber) = SnailNumber.Pair(a, b)

  let addAndReduce a b =
    add a b
    |> reduce

  let rec magnitude sn =
    match sn with
    | Simple v -> (int64 v)
    | Pair (left, right) -> ((int64 3) * (magnitude left)) + ((int64 2) * (magnitude right))

let readInput path =
  System.IO.File.ReadAllLines(path)
  |> List.ofArray
  |> List.map (SnailNumber.fromString)

let part1 path =
  let answer =
    readInput path
    |> List.reduce (SnailNumber.addAndReduce)

  let magnitude = SnailNumber.magnitude answer

  printfn  " │ "
  printfn  " │ Part 1: "
  printfn $" │ %s{path}"
  printfn  " │ "
  printfn $" │ Reduced snail number: %s{ string answer }"
  printfn $" | Magnitude {magnitude}"
  printfn  " └────────────"

open System.Collections.Generic
open System.Linq

let part2 path =
  let numbers = readInput path |> Seq.ofList

  let pairs =
    Seq.allPairs numbers numbers
    |> Seq.where (fun (a, b) -> a <> b)

  let maxMagnitude =
    pairs
      .AsParallel()
      .Select(fun (a, b) -> SnailNumber.addAndReduce a b)
      .Select(SnailNumber.magnitude)
      .OrderByDescending(id)
      .First()

  printfn  " │ "
  printfn  " │ Part 2: "
  printfn $" │ %s{path}"
  printfn  " │ "
  printfn $" │ "
  printfn $" | Max Magnitude - {maxMagnitude}"
  printfn  " └────────────"

part1 "test_input.txt"
part1 "test_input_2.txt"
part1 "input.txt"

part2 "test_input.txt"
part2 "test_input_2.txt"
part2 "input.txt"