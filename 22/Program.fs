open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %A"
    printfn "(accepted answer: 32412)"

    //solve2 input
    //|> printfn "solution 2: %A"
    //printfn "(accepted answer: ?)"

    0
