open System.IO
open Solution

[<EntryPoint>]
let main argv =

    File.ReadAllText "input.txt"
    |> solve
    |> Set.count
    |> printfn "solution: %d"
    printfn "(must be 337)"

    File.ReadAllText "input.txt"
    |> solve2
    |> printfn "solution 2: %d"
    printfn "(48888 is too low)"
    printfn "(48889 is too low)"

    0
