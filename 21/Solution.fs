module Solution

open System
open System.Text.RegularExpressions

type Rule = { Ingredients:string Set; Allergenes:string Set }
type Result = Map<string, string Set>

let splitStr (delimiter:string) (input:string) =
    input.Split([|delimiter|], StringSplitOptions.None) |> Array.toList

let toTuple = function
    | [a;b] -> a,b
    | x     -> failwithf "unexpected: %A" x

let words (input:string) =
    [ for m in Regex.Matches(input, @"\w+") do yield m.Groups.[0].Value ]

let parseLine (line:string) =
    let ins, als =
        splitStr "(contains" line
        |> List.map words
        |> toTuple
    { Ingredients = Set ins; Allergenes = Set als}

let parse (input:string) =
    input.Split '\n'
    |> Array.toList
    |> List.map parseLine


let analyze (rules:Rule list) : Result =
    [   for r in rules do
        for a in r.Allergenes do
            yield a, r.Ingredients ]
    |> List.fold (fun acc (allergene, ingredients) -> 
        let xs =
            acc
            |> Map.tryFind allergene
            |> Option.map (Set.intersect ingredients)
            |> Option.defaultValue ingredients
        acc.Add(allergene, xs)) Map.empty

let options (result:Result) = seq {
    let sorted = result |> Map.toSeq |> Seq.sortBy (snd >> Set.count)
    for allergene, ingredients in sorted do
        if ingredients.Count > 1 then
            for x in ingredients do
                yield allergene, x }
