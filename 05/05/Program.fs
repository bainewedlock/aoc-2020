open System
open System.IO
open System.Text.RegularExpressions

let seatIds =
    File.ReadAllLines @"input.txt"
    |> Seq.map (fun line -> Regex.Replace(line, "B|R", "1"))
    |> Seq.map (fun line -> Regex.Replace(line, "F|L", "0"))
    |> Seq.map (fun line -> Convert.ToInt32(line, 2))

let firstMissing xs =
    let rec loop (x::y::tl) =
        if x + 1 = y
        then loop (y::tl)
        else x + 1
    loop xs

[<EntryPoint>]
let main argv =

    printfn "solution 1: %d" (seatIds |> Seq.max)
    printfn "(must be 994)"

    seatIds
    |> Seq.sort
    |> Seq.toList
    |> firstMissing
    |> printfn "solution 2: %d"
    printfn "(must be 741)"

    0
