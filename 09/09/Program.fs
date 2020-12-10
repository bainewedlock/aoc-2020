open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve 25
    |> printfn "solution: %d"
    printfn "(my answer: 675280050)"

    File.ReadAllText "input.txt"
    |> solve2 675280050L
    |> fun (a, b) -> printfn "solution: %d + %d = %d" a b (a+b)
    printfn "(my answer: 96081673)"

    0
