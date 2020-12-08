module Solution
open System.Text.RegularExpressions
open System.Collections.Generic

type Record = {
    bag : string
    content : (int * string) list }

let regexMatches pattern line = [
    for m in Regex.Matches(line, pattern) do
        yield m.Groups.[1].Value ]

let toTuple = function
    | []     -> failwithf "empty list"
    | hd::tl -> hd, tl

let parseLine (line:string) =
    let hd, tl =
        regexMatches @"(\w+ \w+) bag" line
        |> List.filter ((<>)"no other")
        |> toTuple
    let ns     =
        regexMatches @"(\d+)" line
        |> List.map int
    { bag = hd; content = List.zip ns tl }

let private toNode record =
    record.content
    |> List.map (fun (_, inner) -> inner, record.bag)

let graph records =
    records
    |> List.collect toNode
    |> List.groupBy fst
    |> List.map (fun (g, xs) -> g, List.map snd xs)
    |> Map

let parse (input:string) =
    input.Trim().Split '\n'
    |> Array.toList
    |> List.map ((fun x -> x.Trim()) >> parseLine)

// based on "Breadth First Search"
let discover look (start:'node) =
    let discovered = HashSet<'node>([start])
    let rec loop queue = seq {
        match queue with
        | []     -> ()
        | hd::tl ->
            yield hd
            let ns = look hd |> List.filter (discovered.Contains >> not)
            ns |> List.iter (discovered.Add >> ignore)
            yield! loop (tl @ ns) }
    loop [start]

let solve input =
    let needle = "shiny gold"
    let g = input |> parse |> graph
    let look = g.TryFind >> Option.defaultValue []
    discover look needle
    |> Set
    |> Set.remove needle

