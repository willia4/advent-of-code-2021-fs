namespace CommonHelpers

module Helpers =
    
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
        
    let countOccurrences (values: seq<'a>) =
        values |> Seq.fold (fun (acc: Map<'a, int>) next ->
            acc |> Map.change next (fun c ->
                    match c with
                    | Some v -> Some (v + 1)
                    | None -> Some (1))) Map.empty
