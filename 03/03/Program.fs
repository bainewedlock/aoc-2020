open System.IO


[<EntryPoint>]
let main argv =

    Solution.solve (File.ReadAllText "input.txt")
    |> printfn "solution 1: %d"
    printfn "must be 262"

    Solution.solve2 (File.ReadAllText "input.txt")
    |> printfn "solution 2: %d"
    printfn "must be 2698900776"

    0
