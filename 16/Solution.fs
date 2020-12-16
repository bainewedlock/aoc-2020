module Solution
open System.Text.RegularExpressions

type Rule = {
    name   : string
    ranges : (int * int) list }

module Rule =
    let create name a b c d = { name=name; ranges=[a,b; c,d] }

type Notes = {
    nearbyTickets : int list
    rules         : Rule list }

let regexMatchDict pattern input =
    let m = Regex.Match(input, pattern)
    dict [ for g in m.Groups do yield g.Name, g.Value].Tail

let regexGroups pattern input =
    let ms = Regex.Matches(input, pattern)
    [ for m in ms do yield [ for g in m.Groups do yield g.Value ].Tail ]

let regexReplace pattern (replacement:string) input =
    Regex.Replace(input, pattern, replacement)

let split (delimiter:string) (text:string) =
    text.Split delimiter
    |> Array.toList

let toTuple = function
    | [a;b] -> a, b
    | x     -> failwithf "unexpected: %A" x

let parse (input:string) =
    let p = @"^(?<ru>.+) your ticket: (?<yt>.+) nearby tickets: (?<nt>.+)"
    let d =
        input
        |> regexReplace @"[\r\n,]+" " "
        |> regexMatchDict p
    {   nearbyTickets = d.["nt"] |> split " " |> List.map int
        rules =
            d.["ru"]
            |> regexGroups @"(\w+): (\d+)-(\d+) or (\d+)-(\d+)"
            |> List.map (fun (name::a::b::c::d::_) ->
                Rule.create name (int a) (int b) (int c) (int d)) }

let inRange no (min, max) = min <= no && no <= max

let validField (rules:Rule list) no = 
    rules
    |> List.collect (fun r -> r.ranges)
    |> List.exists (inRange no)

let solve input =
    let notes = parse input
    notes.nearbyTickets
    |> Seq.filter (validField notes.rules >> not)
    |> Seq.sum

