module Solution

open System.Text.RegularExpressions

let parse (input:string) =
    input.Split '\n'
    |> Seq.map ((fun x -> x.Trim()) >> int)
    |> Seq.sort
    |> Seq.toList

let preAnalyze = function
    | []       -> failwith "unexpected"
    | hd::tail ->
        tail
        |> List.fold (fun (prev, history) x -> 
            let d = x - prev
            (x, d::history))
                (hd, [])
        |> snd
        |> List.map string
        |> String.concat ""

let analyze =
    preAnalyze
    >> fun s -> Regex.Replace(s, "[^1]", " ").Trim().Split()
    >> Array.toList
    >> List.map (fun x -> x.Length)
    >> List.filter ((<) 0)
    >> List.map ((+) 1)
    >> List.sort

let simulateEnds xs = [0] @ xs @ [xs |> List.max |> ((+)3) ]

let solve (input:string) =
    let data   = parse input |> simulateEnds |> preAnalyze
    let ones   = data |> Seq.filter ((=) '1') |> Seq.length
    let threes = data |> Seq.filter ((=) '3') |> Seq.length
    ones, threes

// the input only contains chains of 1-5 numbers
// and for example, its possible to arrange 5 numbers in 7 valid ways
let combinations = function
    | 1 -> 1
    | 2 -> 1
    | 3 -> 2
    | 4 -> 4
    | 5 -> 7
    | _ -> failwithf "unexpected"

let solve2 = 
    parse
    >> simulateEnds
    >> analyze
    >> List.map combinations
    >> List.map int64
    >> List.reduce (*)

