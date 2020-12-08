open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve
    |> Set.count
    |> printfn "solution: %d"
    printfn "(accepted answer: 337)"

    File.ReadAllText "input.txt"
    |> solve2
    |> List.sumBy fst
    |> printfn "solution 2: %d"
    printfn "(accepted answer: 50100)"

    0
