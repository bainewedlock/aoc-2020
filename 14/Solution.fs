module Solution
open System
open System.Text.RegularExpressions

type Instruction =
    | Mask of string
    | Mem of (int * int)

let tryRegexMatch pattern input =
    match Regex.Match(input, pattern).Groups |> Seq.toList with
    | [] | [_] -> None
    | _::tl    -> tl |> List.map (fun x -> x.Value) |> Some

let toTuple = function
    | [a; b] -> a, b
    | x      -> failwithf "unexpected %A" x

let tryParseMask =
    tryRegexMatch @"mask = (\w+)"
    >> Option.map (List.exactlyOne >> Mask)

let tryParseMem =
    tryRegexMatch @"mem\[(\d+)\] = (\d+)"
    >> Option.map (List.map int >> toTuple >> Mem)

let parseLine (line:string) =
    [ tryParseMask; tryParseMem ]
    |> List.pick (fun f -> f line)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map parseLine

type State = {
    mask : string
    mem  : Map<int, string> }

let maskbit m b = if m = 'X' then b else m

let mask (m:string) (x:int) =
    Convert.ToString(x, 2).PadLeft(m.Length, '0')
    |> Seq.map2 maskbit m
    |> Seq.toArray
    |> System.String

let exec state op =
    match op with
    | Mask m     -> { state with mask = m }
    | Mem (a, x) -> { state with mem = state.mem.Add(a, mask state.mask x) }

let solve input =
    parse input
    |> List.fold exec { mask=""; mem=Map.empty }
    |> fun s -> s.mem
    |> Seq.map (fun k -> Convert.ToUInt64(k.Value, 2))
    |> Seq.sum

