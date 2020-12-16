module Solution
open System.Text.RegularExpressions

type Rule = {
    name   : string
    ranges : (int * int) list }

type Ticket = int list

module Rule =
    let create name a b c d = { name=name; ranges=[a,b; c,d] }

type Notes = {
    nearbyTickets : Ticket list
    yourTicket    : Ticket
    rules         : Rule list }

type Config = Map<string, Set<int>>

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

let parseTicket = regexSplit "," >> List.map int

let parse (input:string) =
    let p = @"^(?<ru>.+)#yourticket:#(?<yt>.+)#nearbytickets:#(?<nt>.+)"
    let d =
        input.Trim()
        |> regexReplace " " ""
        |> regexReplace @"[\r\n]+" "#"
        |> regexMatchDict p
    let yt = d.["yt"] |> parseTicket
    let nt = d.["nt"] |> regexSplit "#" |> List.map parseTicket
    let ru =
        d.["ru"]
        |> regexGroups @"(\w+):(\d+)-(\d+)or(\d+)-(\d+)"
        |> List.map (function 
            | name::a::b::c::d::_ ->
                Rule.create name (int a) (int b) (int c) (int d)
            | x -> failwithf "unexpected %A" x)
    { nearbyTickets = nt; rules = ru; yourTicket = yt }

let inRange no (min, max) = min <= no && no <= max

let validField (rules:Rule list) fieldValue = 
    rules
    |> List.collect (fun r -> r.ranges)
    |> List.exists (inRange fieldValue)

let solve input =
    let notes = parse input
    notes.nearbyTickets
    |> List.concat
    |> Seq.filter (validField notes.rules >> not)
    |> Seq.sum

let testRule rule tickets fieldIndex =
    tickets
    |> Seq.map (Array.item fieldIndex)
    |> Seq.forall (validField [rule])

let analyze rules (tickets:Ticket list) : Config =
    let ts = tickets |> List.map (List.toArray)
    let fieldIndices = Set [ 0 .. tickets.Head.Length-1 ]
    rules
    |> List.map (fun r ->
        r.name, fieldIndices |> Set.filter (testRule r ts))
    |> Map

let validTickets notes =
    notes.yourTicket::notes.nearbyTickets
    |> List.filter (Seq.forall (validField notes.rules))

let pick (name, fieldIndex:int) =
    Map.map (fun k v ->
        if k = name then Set [fieldIndex]
        else v |> Set.remove fieldIndex)

let possibleConfig : Config -> bool =
    Map.forall (fun _ v -> not v.IsEmpty)

let solution : Config -> bool =
    Map.forall (fun _ v -> v.Count = 1)

let rec searchSolution (config:Config) =
    if solution config then Some config else
    if not (possibleConfig config) then None else
    let options =
        config
        |> Seq.filter (fun x -> x.Value.Count > 1)
        |> Seq.sortBy (fun x -> x.Value.Count)
        |> Seq.collect (fun x -> x.Value |> Seq.map(fun y -> x.Key, y))
    options |> Seq.tryPick (fun o -> searchSolution (pick o config))

let describeTicket (ticket:Ticket) (config:Config) =
    config
    |> Map.map (fun _ v -> ticket |> List.item (v |> Seq.exactlyOne))

let preSolve input =
    let notes = parse input
    let config = analyze notes.rules (validTickets notes)
    searchSolution config
    |> Option.get
    |> describeTicket notes.yourTicket

let solve2 =
    preSolve
    >> Map.filter (fun k v -> k.StartsWith "departure")
    >> Seq.map (fun k -> k.Value)
    >> Seq.map int64
    >> Seq.reduce (*)
