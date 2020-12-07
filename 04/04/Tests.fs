module Tests

open Swensen.Unquote
open Expecto
open Solution

[<Tests>]
let tests = 
    testList "tests" [
        test "solve" {
            solve demoInput =! 2 }
        test "eval" {
            Set ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "cid"; "hgt"]
            |> eval =! 7 }
        test "parse" {
            demoRecords =! [
                Set ["ecl"; "pid"; "eyr"; "hcl"; "byr"; "iyr"; "cid"; "hgt"]
                Set ["iyr"; "ecl"; "cid"; "eyr"; "pid"; "hcl"; "byr" ]
                Set ["hcl"; "iyr"; "eyr"; "ecl"; "pid"; "byr"; "hgt"]
                Set ["hcl"; "eyr"; "pid"; "iyr"; "ecl"; "hgt"] ] }
        test "splitLines" {
            [ "a"; "b"; ""; "c"; ""; "d"; "e" ]
            |> splitLines ((=)"") =!
                [["a";"b"];["c"];["d";"e"]] }
        test "parseLine" {
            parseLine "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd" =!
                Set [
                    "ecl"//, "gry"
                    "pid"//, "860033327"
                    "eyr"//, "2020"
                    "hcl"//, "#fffffd" ]
                ]
        }
    ]

