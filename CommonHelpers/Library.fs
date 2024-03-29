﻿namespace CommonHelpers

module Helpers =
    let fst3 (a, _, _) = a

    let safeParseInt (s:string) =
        match System.Int32.TryParse s with
        | true, int -> int
        | _ -> 0

    let charStrings (s:string) = seq {
        for i in seq { 0..(s.Length - 1) } do
            yield s.Substring(i, 1)
    }
    
    let reverseString (s:string) =
        new string(s.ToCharArray() |> Array.rev)

    let trimString (s:string) = s.Trim()
            
    let stringIsEmpty (s:string) = System.String.IsNullOrWhiteSpace(s)
    
    let stringNotEmpty = stringIsEmpty >> not
    
    let splitBy (sep: string) (s: string) = s.Split(sep)

    let split (s: string) = s.Split()

    let stringToUpper (s: string) = s.ToUpperInvariant()
    let stringToLower (s: string) = s.ToLowerInvariant()

    let countOccurrences (values: seq<'a>) =
        values |> Seq.fold (fun (acc: Map<'a, int>) next ->
            acc |> Map.change next (fun c ->
                    match c with
                    | Some v -> Some (v + 1)
                    | None -> Some (1))) Map.empty

    let countSeq = seq {
        let mutable i = 0
        while true do
            yield i
            i <- i + 1
    }
    
    let chunkBySeparator (isSep: 'a -> bool) (items: 'a seq): seq<seq<'a>> = seq {
        let currentChunk = new ResizeArray<'a>()
        
        for i in items do
            if isSep i then
                if currentChunk.Count > 0 then
                    yield (currentChunk |> Seq.toList)
                    currentChunk.Clear()
                else ()
            else
                currentChunk.Add(i)
                
        if currentChunk.Count > 0 then
            yield (currentChunk |> Seq.toList)
        else ()
    }
        
    let seqAsSlidingWindow windowSize (items: 'a seq) = seq {
        let currentChunk = new ResizeArray<'a>()
        
        for i in items do
            if (currentChunk.Count = windowSize) then
                yield currentChunk |> Seq.toList |> Seq.ofList
                currentChunk.RemoveAt(0)
            else ()
            
            currentChunk.Add(i)
            
        if (currentChunk.Count = windowSize) then
            yield currentChunk |> Seq.toList |> Seq.ofList
        else ()
    }
        
    // like "Seq.takeWhile" except that the output sequence includes  the first item that yielded false
    let takeUntil (f: 'a -> bool) (items: 'a seq) = seq {
        let mutable hadFailure = false
        for i in items do
            if (hadFailure = false) then
                yield i
                hadFailure <- hadFailure || (f i)
            else ()
    }
        
    let sumUnsignedLong (s: seq<uint64>) =
        s |> Seq.reduce (fun a b -> a + b)