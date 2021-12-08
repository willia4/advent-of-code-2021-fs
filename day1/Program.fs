
let readInput path =
    path
    |> System.IO.File.ReadAllLines
    |> Array.map CommonHelpers.Helpers.safeParseInt

let part1 (path:string) : unit =
    let inputs = path
                 |> readInput
                 |> Seq.fold (fun values nextValue -> values) (0, 0)
    
    //|> Seq.iter (fun x -> printfn $"%d{x}")
    
    
    
part1 "test_input.txt"
