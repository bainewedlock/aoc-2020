open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %d"
    printfn "(accepted answer: 5902420735773)"

    solve2 input
    |> printfn "solution 2: %d"
    printfn "(accepted answer: 3801988250775)"

    0
