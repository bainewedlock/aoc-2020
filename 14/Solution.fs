module Solution
open System
open System.Text.RegularExpressions

type Instruction =
    | Mask of string
    | Mem of (int64 * int64)

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
    >> Option.map (List.map int64 >> toTuple >> Mem)

let parseLine (line:string) =
    [ tryParseMask; tryParseMem ]
    |> List.pick (fun f -> f line)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map parseLine

type State = {
    mask : string
    mem  : Map<int64, int64> }

let maskBit ignore m b = if m = ignore then b else m

let toBits (value:int64) = Convert.ToString(value, 2)
let fromBits (bits:string) = Convert.ToInt64(bits, 2)

let maskString maskFunc (mask:string) (value:string) =
    value.PadLeft(mask.Length, '0')
    |> Seq.map2 maskFunc mask
    |> Seq.toArray
    |> System.String

let maskValue (mask:string) value =
    value |> toBits |> maskString (maskBit 'X') mask |> fromBits

let maskAddress (mask:string) (addr:int64) =
    addr |> toBits |> maskString (maskBit '0') mask

let exec state op =
    match op with
    | Mask m     -> { state with mask = m }
    | Mem (a, x) ->
        { state with mem = state.mem.Add(a, maskValue state.mask x) }

let solve input =
    parse input
    |> List.fold exec { mask=""; mem=Map.empty }
    |> fun s -> s.mem
    |> Seq.sumBy (fun k -> k.Value)

let setMultiple value m addresses =
    addresses
    |> List.fold (fun (m':Map<int64,int64>) x -> m'.Add(x, value)) m

let replaceAt index c (text:string) =
    text
    |> Seq.mapi (fun i x -> if i = index then c else x)
    |> Seq.toArray
    |> System.String

let calcAddresses (pattern:string) = 
    printfn "yey"
    let rec loop depth (p':string) = [
        if depth > 100 then failwith "depth exceeded"
        match p'.IndexOf "X" with
        | -1 -> yield Convert.ToInt64(p', 2)
        |  i -> yield! loop (depth+1) (p' |> replaceAt i '0')
                yield! loop (depth+1) (p' |> replaceAt i '1') ]
    loop 0 pattern

let exec2 state op =
    match op with
    | Mask m     -> { state with mask = m }
    | Mem (a, x) ->
        let mem' =
            maskAddress state.mask a
            |> calcAddresses
            |> setMultiple x state.mem
        { state with mem = mem' }

let solve2 input =
    parse input
    |> List.fold exec2 { mask=""; mem=Map.empty }
    |> fun s -> s.mem
    |> Seq.sumBy (fun k -> k.Value)

