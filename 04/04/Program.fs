open System.IO

[<EntryPoint>]
let main argv =
    File.ReadAllText "input.txt"
    |> Solution.solve
    |> printfn "solution: %d"
    printfn "must be 204"


    0
