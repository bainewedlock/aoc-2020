module Solution
open System.Text.RegularExpressions

let parseLine (line:string) =
    Regex.Matches(line, "([a-z]{3}):\S+")
    |> Seq.map (fun x -> x.Groups.[1].Value)
    |> Set

let splitLines predicate xs =
    List.foldBack (fun x (hd::tl) ->
        if predicate x
        then ([]::(hd::tl))
        else ((x::hd)::tl)) xs [[]]

let demoInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                 byr:1937 iyr:2017 cid:147 hgt:183cm
                 
                 iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                 hcl:#cfa07d byr:1929
                 
                 hcl:#ae17e1 iyr:2013
                 eyr:2024
                 ecl:brn pid:760753108 byr:1931
                 hgt:179cm
                 
                 hcl:#cfa07d eyr:2025 pid:166559648
                 iyr:2011 ecl:brn hgt:59in"

let parse (input:string) = 
    input.Split '\n'
    |> Array.toList
    |> List.map (fun x -> x.Trim())
    |> splitLines ((=) "")
    |> List.map (fun xs -> xs |> String.concat " " |> parseLine)

let demoRecords = parse demoInput

let eval xs =
    let req = Set [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    Set.intersect xs req
    |> Set.count

let solve =
    parse 
    >> List.filter (eval >> ((=) 7))
    >> List.length

