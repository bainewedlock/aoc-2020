module Tests

open Swensen.Unquote
open Expecto
open Solution
open Demoinput

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
        test "parse input" {
            let records = parse demoinput
            records.Length =! 9 }
        test "parse line" {
            parseLine "faded blue bags contain no other bags." =!
                { bag = "faded blue"; content = [] }
            parseLine ("light red bags contain 1 bright white bag," +
                " 2 muted yellow bags.") =!
                { bag="light red"; content=[1, "bright white"
                                            2, "muted yellow"]} } ]
