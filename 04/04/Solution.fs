module Solution
open System.Text.RegularExpressions
open System.Collections.Generic

type FieldState =
    | Valid
    | InvalidFields of (string list)

type Record = IDictionary<string, string>

let parseLine (line:string) =
    Regex.Matches(line, "([a-z]{3}):(\S+)")
    |> Seq.map (fun x -> x.Groups.[1].Value, x.Groups.[2].Value)
    |> dict

let splitLines predicate xs =
    List.foldBack (fun x acc ->
        match acc with 
        | []     -> failwith "unexpected"
        | hd::tl ->
            if predicate x
            then ([]::(hd::tl))
            else ((x::hd)::tl)) xs [[]]

let parse (input:string) = 
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> splitLines ((=) "")
    |> List.map (fun xs -> xs |> String.concat " " |> parseLine)

let countFields (xs:IDictionary<string, string>) =
    let keys = xs.Keys |> Set
    let req = Set [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    Set.intersect keys req
    |> Set.count

let toInt (x:string) =
    match System.Int32.TryParse x with
    | true, value -> Some value
    | _           -> None

let extractInt (pattern:string) (x:string) =
    match Regex.Match(x, pattern) with
    | m when m.Groups.Count = 2 -> Some (int m.Groups.[1].Value)
    | _                         -> None

let checkRange min max = function
    | Some v when min <= v && v <= max -> true
    | _                                -> false

let checkRegex pattern x = Regex.IsMatch(x, pattern)

let any (fs:(string -> bool) list)  (value:string) =
    fs |> List.filter (fun f -> f value) |> List.isEmpty |> not

let rules = [
    "byr", toInt >> checkRange 1920 2002
    "iyr", toInt >> checkRange 2010 2020
    "eyr", toInt >> checkRange 2020 2030
    "hgt", any [extractInt "^(\d+)cm$" >> checkRange 150 193
                extractInt "^(\d+)in$" >> checkRange 59 76]
    "hcl", checkRegex "^#[0-9a-f]{6}$"
    "ecl", checkRegex "^amb|blu|brn|gry|grn|hzl|oth$"
    "pid", checkRegex "^[0-9]{9}$" ]

let validate (r:Record) =
    let isError (key, rule) = r.ContainsKey key && not <| rule r.[key]
    rules
    |> List.filter isError
    |> List.map fst
    |> function
    | [] -> Valid
    | xs -> InvalidFields xs

let solve =
    parse 
    >> List.filter (countFields >> ((=) 7))
    >> List.length

let solve2 =
    parse
    >> List.filter (countFields >> ((=) 7))
    >> List.filter (validate >> ((=) Valid))
    >> List.length
