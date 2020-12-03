open System.IO
open Newtonsoft.Json

let toJson x = JsonConvert.SerializeObject(x, Formatting.Indented)

[<EntryPoint>]
let main argv =

    Solution.solve (File.ReadAllText "input.txt")
    |> Seq.filter snd
    |> Seq.length
    |> printfn "solution: %d"
    printfn "must be 620"

    Solution.solve2 (File.ReadAllText "input.txt")
    |> Seq.filter snd
    |> Seq.length
    |> printfn "solution 2: %d"
    printfn "must be 727"

    0
