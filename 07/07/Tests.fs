module Tests

open System.IO
open Swensen.Unquote
open Expecto
open Solution
open Demoinput

[<Tests>]
let solve2Tests =
    testList "solve2" [
        test "demoinput" {
            solve2 demoinput =! 32 }
        test "demoinput #2" {
            "shiny gold bags contain 2 dark red bags.
             dark red bags contain 2 dark orange bags.
             dark orange bags contain 2 dark yellow bags.
             dark yellow bags contain 2 dark green bags.
             dark green bags contain 2 dark blue bags.
             dark blue bags contain 2 dark violet bags.
             dark violet bags contain no other bags."
            |> solve2 =! 126 } ]

[<Tests>]
let graph2Tests =
    testList "graph2" [
        test "demoinput" {
            let g = graph2 demoinput
            g.Item "light red" =! [
                1, "bright white"
                2, "muted yellow" ]
            g.Count =! 9 } ]

[<Tests>]
let solveTests =
    testList "solve" [
        test "demoinput" {
            solve demoinput =! Set [
                "bright white"
                "muted yellow"
                "dark orange"
                "light red"] }
        test "discover" {
            let g = dict [
                1, [2;10]
                3, [4]
                2, [5]
                10, []]
            let look x = if g.ContainsKey x then g.Item x else []
            discover look 1 |> Seq.toList =! [ 1; 2; 10; 5 ] } ]

[<Tests>]
let graphTests =
    testList "graph" [
        test "some bags" {
            graph [
                { bag = "red";    content = [ 1, "yellow" ]}
                { bag = "yellow"; content = [ 1, "red" ]}
                { bag = "yellow"; content = [ 1, "green" ]}
                { bag = "red";    content = [ 1, "green" ]} ]
                =! Map [
                    "yellow", ["red"]
                    "red", ["yellow"]
                    "green", ["yellow"; "red"] ] } ]

[<Tests>]
let parseTests =
    testList "parse" [
        test "input count" {
            File.ReadAllText "input.txt"
            |> parse
            |> List.length
                =! 594 }
        test "demoinput" {
            let records = parse demoinput
            records.Length =! 9 }
        test "line" {
            parseLine "faded blue bags contain no other bags." =!
                { bag = "faded blue"; content = [] }
            parseLine ("light red bags contain 1 bright white bag," +
                " 2 muted yellow bags.") =!
                { bag="light red"; content=[1, "bright white"
                                            2, "muted yellow"]} } ]
