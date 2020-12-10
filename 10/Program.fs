open Solution

let input = System.IO.File.ReadAllText "input.txt"

[<EntryPoint>]
let main argv =

    solve input
    |> fun (a, b) -> printfn "solution 1: %d * %d = %d" a b (a*b)
    printfn "(accepted answer: 2030)"

    0
