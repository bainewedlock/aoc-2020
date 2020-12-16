module Solution
open System.Text.RegularExpressions

type Rule = {
    name   : string
    ranges : (int * int) list }

module Rule =
    let create name a b c d = { name=name; ranges=[a,b; c,d] }

type Notes = {
    nearbyTickets : int list list
    rules         : Rule list }

let regexMatchDict pattern input =
    let m = Regex.Match(input, pattern)
    dict [ for g in m.Groups do yield g.Name, g.Value].Tail

let regexGroups pattern input =
    let ms = Regex.Matches(input, pattern)
    [ for m in ms do yield [ for g in m.Groups do yield g.Value ].Tail ]

let regexReplace pattern (replacement:string) input =
    Regex.Replace(input, pattern, replacement)

let regexSplit (delimiter:string) (text:string) =
    Regex.Split(text, delimiter) |> Array.toList

let toTuple = function
    | [a;b] -> a, b
    | x     -> failwithf "unexpected: %A" x

let parse (input:string) =
    let p = @"^(?<ru>.+)#yourticket:#(?<yt>.+)#nearbytickets:#(?<nt>.+)"
    let d =
        input.Trim()
        |> regexReplace " " ""
        |> regexReplace @"\r\n" "#"
        |> regexMatchDict p
    let nt =
        d.["nt"]
        |> regexSplit "#"
        |> List.map (regexSplit "," >> List.map int)
    let ru =
        d.["ru"]
        |> regexGroups @"(\w+):(\d+)-(\d+)or(\d+)-(\d+)"
        |> List.map (function 
            | name::a::b::c::d::_ ->
                Rule.create name (int a) (int b) (int c) (int d)
            | x -> failwithf "unexpected %A" x)
    { nearbyTickets = nt; rules = ru }

let inRange no (min, max) = min <= no && no <= max

let validField (rules:Rule list) no = 
    rules
    |> List.collect (fun r -> r.ranges)
    |> List.exists (inRange no)

let solve input =
    let notes = parse input
    notes.nearbyTickets
    |> List.concat
    |> Seq.filter (validField notes.rules >> not)
    |> Seq.sum

