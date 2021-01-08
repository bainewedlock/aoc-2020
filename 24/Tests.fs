module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "solve" [
            test "demoinput" { solve demoinput =! 10 }]
        testList "walking" [
            test "adjacent to reference" {
                let path = parse "esew" |> List.exactlyOne
                walk path =! (0, 1, -1) }
            test "back to reference" {
                let path = parse "nwwswee" |> List.exactlyOne
                walk path =! (0, 0, 0) } ]
        testList "tools" [
            test "convert direction to offset" {
                offset.Item "e" =! (1, 0, -1) } ]
        testList "parsing" [
            test "demoinput" {
                let ps = parse demoinput 
                ps.Length =! 20 }
            test "a line" { parseLine "esew" =! [ "e"; "se"; "w" ] } ] ]


