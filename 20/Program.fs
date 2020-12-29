open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> printfn "solution 1: %d"
    printfn "(accepted answer: 17032646100079)"


    //solve2 input
    //|> printfn "solution 2: %A"
    //printfn "(accepted answer: ?)"

    0
