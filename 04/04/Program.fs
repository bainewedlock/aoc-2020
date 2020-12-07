open System.IO

[<EntryPoint>]
let main argv =
    File.ReadAllText "input.txt"
    |> Solution.solve
    |> printfn "solution: %d"
    printfn "must be 204"

    File.ReadAllText "input.txt"
    |> Solution.solve2
    |> printfn "solution 2: %d"
    printfn "must be 179"

    0
