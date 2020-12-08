open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve
    |> Set.count
    |> printfn "solution: %d"
    printfn "(must be 337)"

    0
