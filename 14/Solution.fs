module Solution
open System
open System.Text.RegularExpressions

type Address = int64
type Value   = int64
type Memory = Map<Address, Value>

type Instruction =
    | Mask of string
    | Mem of (Address * Value)

type State = {
    mask : string
    mem  : Memory }

let tryRegexMatch pattern input =
    match Regex.Match(input, pattern).Groups |> Seq.toList with
    | [] | [_] -> None
    | _::tl    -> tl |> List.map (fun x -> x.Value) |> Some

let private toTuple = function
    | [a; b] -> a, b
    | x      -> failwithf "unexpected %A" x

let private tryParseMask =
    tryRegexMatch @"mask = (\w+)"
    >> Option.map (List.exactlyOne >> Mask)

let private tryParseMem =
    tryRegexMatch @"mem\[(\d+)\] = (\d+)"
    >> Option.map (toTuple >> fun (a, b) -> Mem (int64 a, int64 b))

let private parseLine (line:string) =
    [ tryParseMask; tryParseMem ]
    |> List.pick (fun f -> f line)

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map parseLine

let private toBits (value:Value) = Convert.ToString(value, 2)
let private fromBits (bits:string) = Convert.ToInt64(bits, 2)
let private maskBit ignore m b = if m = ignore then b else m

let maskString fMask (mask:string) (value:string) =
    value.PadLeft(mask.Length, '0')
    |> Seq.map2 fMask mask
    |> Seq.toArray
    |> System.String

let maskValue (mask:string) value =
    value |> toBits |> maskString (maskBit 'X') mask |> fromBits

let maskAddress (mask:string) (addr:Address) =
    addr |> toBits |> maskString (maskBit '0') mask

let setMultiple value mem addresses =
    addresses
    |> List.fold (fun (mem':Memory) x -> mem'.Add(x, value)) mem

let replaceAt index c (text:string) =
    text
    |> Seq.mapi (fun i x -> if i = index then c else x)
    |> Seq.toArray
    |> System.String

let rec computeAddresses (pattern:string) = [
    match pattern.IndexOf "X" with
    | -1 -> yield fromBits pattern
    |  i -> yield! computeAddresses (pattern |> replaceAt i '0')
            yield! computeAddresses (pattern |> replaceAt i '1') ]

let exec state op =
    match op with
    | Mask m     -> { state with mask = m }
    | Mem (a, x) ->
        let mem' = state.mem.Add(a, maskValue state.mask x)
        { state with mem = mem' }

let exec2 state op =
    match op with
    | Mask m     -> { state with mask = m }
    | Mem (a, x) ->
        let addresses = maskAddress state.mask a |> computeAddresses
        let mem' = setMultiple x state.mem addresses
        { state with mem = mem' }

let private genericSolve fExec input =
    parse input
    |> List.fold fExec { mask=""; mem=Map.empty }
    |> fun s -> s.mem
    |> Seq.sumBy (fun k -> k.Value)

let solve  = genericSolve exec
let solve2 = genericSolve exec2
