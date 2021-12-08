namespace CommonHelpers

module Helpers =
    let safeParseInt (s:string) =
        match System.Int32.TryParse s with
        | true, int -> int
        | _ -> 0

