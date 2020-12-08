open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve
    |> printfn "Solution: %d"
    printfn "(must be 1586)"

    File.ReadAllText "input.txt"
    |> solve2
    |> printfn "Solution: %d"
    printfn "(must be 703)"

    0
