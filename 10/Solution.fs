module Solution

open System.Text.RegularExpressions

let parse (input:string) =
    input.Split '\n'
    |> Seq.map ((fun x -> x.Trim()) >> int)

let solve (input:string) =
    parse input
    |> Seq.sort
    |> Seq.fold (fun (prev, history:Map<int,int>) x -> 
        let d = x - prev
        let sum = history.TryFind d |> Option.defaultValue 0
        (x, history.Add(d, sum+1)))
            (0, Map.empty)
    |> snd
    |> fun m -> m.[1], m.[3] + 1

let analyze = function
    | []       -> failwith "unexpected"
    | hd::tail ->
    tail
    |> List.sort
    |> List.fold (fun (prev, history) x -> 
        let d = x - prev
        (x, d::history))
            (hd, [])
    |> snd
    |> List.map string
    |> String.concat ""
    |> fun s -> Regex.Replace(s, "[^1]", " ").Trim().Split()
    |> Array.toList
    |> List.map (fun x -> x.Length)
    |> List.filter ((<) 0)
    |> List.map ((+) 1)
    |> List.sort

// the input only contains chains of 1-5 numbers
// and for example, its possible to arrange 5 numbers in 7 valid ways
let combinations = function
    | 1 -> 1
    | 2 -> 1
    | 3 -> 2
    | 4 -> 4
    | 5 -> 7
    | _ -> failwithf "unexpected"

let solve2 input = 
    let initial = parse input |> Seq.toList
    let first = 0
    let last  = initial |> List.max |> ((+) 3)
    let all   = [first] @ initial @ [last]
    all
    |> analyze
    |> List.map combinations
    |> List.map int64
    |> List.reduce (*)

