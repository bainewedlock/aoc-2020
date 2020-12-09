open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve 25
    |> printfn "solution: %d"
    printfn "(my answer: 675280050)"

    0
