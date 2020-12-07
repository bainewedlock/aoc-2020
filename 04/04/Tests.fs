module Tests

open Swensen.Unquote
let utest = test
open Expecto
open DemoInput
open Solution

[<Tests>]
let part2tests = 
    let check = parseLine >> validate
    testList "part 2 tests" [

        test "solve2" {
            solve2 demoinput2 =! 4 }

        test "pid is 9 digits" {
            utest <@ check "pid:12345678" = InvalidFields ["pid"] @>
            utest <@ check "pid:000111333" = Valid @> }

        test "ecl is amb|blu|brn|gry|grn|hzl|oth" {
            utest <@ check "ecl:xxx" = InvalidFields ["ecl"] @>
            utest <@ check "ecl:brn" = Valid @> }

        test "hcl is a html color" {
            utest <@ check "hcl:#000000" = Valid @>
            utest <@ check "hcl:#ffffff" = Valid @>
            utest <@ check "hcl:#00000" = InvalidFields ["hcl"] @>
            utest <@ check "hcl:#0000000" = InvalidFields ["hcl"] @> }

        test "hgt between 150cm and 193cm or between 59in and 76in" {
            utest <@ check "hgt:149cm" = InvalidFields ["hgt"] @>
            utest <@ check "hgt:150cm" = Valid @>
            utest <@ check "hgt:194cm" = InvalidFields ["hgt"] @>
            utest <@ check "hgt:58in" = InvalidFields ["hgt"] @>
            utest <@ check "hgt:59in" = Valid @> }
 
        test "iyr between 2010 and 2020" {
            utest <@ check "iyr:1919" = InvalidFields ["iyr"] @> }

        test "byr between 1920 and 2002" {
            utest <@ check "byr:1919" = InvalidFields ["byr"] @>
            utest <@ check "byr:1920" = Valid @>
            utest <@ check "byr:2002" = Valid @>
            utest <@ check "byr:2003" = InvalidFields ["byr"] @> }

        test "eyr between 2020 and 2030" {
            utest <@ check "eyr:1972" = InvalidFields ["eyr"] @>
            utest <@ check "eyr:2019" = InvalidFields ["eyr"] @>
            utest <@ check "eyr:2020" = Valid @>
            utest <@ check "eyr:2030" = Valid @>
            utest <@ check "eyr:2031" = InvalidFields ["eyr"] @> } ]

[<Tests>]
let part1Tests = 
    testList "part 1 tests" [

        test "solve" {
            solve demoInput =! 2 }

        test "countFields" {
            ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "cid"; "hgt"]
            |> List.map (fun x -> x, "dummy")
            |> dict
            |> countFields =! 7 }

        test "parse" {
            parse demoInput |> List.map (fun d -> d.Keys |> Set) =! [
                Set ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "cid"; "hgt"]
                Set ["iyr"; "ecl"; "cid"; "eyr"; "pid"; "hcl"; "byr" ]
                Set ["hcl"; "iyr"; "eyr"; "ecl"; "pid"; "byr"; "hgt"]
                Set ["hcl"; "eyr"; "pid"; "iyr"; "ecl"; "hgt"] ] }

        test "splitLines" {
            [ "a"; "b"; ""; "c"; ""; "d"; "e" ]
            |> splitLines ((=)"") =!
                [["a";"b"];["c"];["d";"e"]] }

        test "parseLine" {
            parseLine "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
            |> Seq.toList =!
                Seq.toList (dict [
                    "ecl", "gry"
                    "pid", "860033327"
                    "eyr", "2020"
                    "hcl", "#fffffd" ]) } ]

