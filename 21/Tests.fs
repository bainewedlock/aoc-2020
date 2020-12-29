module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput
open System
open System.Text.RegularExpressions

[<Tests>]
let all =
    testList "all" [
        //testList "dfs" [
        //    let state = analyze (parse demoinput)
        //    dfs options state
        //]
        testList "options" [
            test "choose small selection first" {
                "a b c (contains e f)\n" +
                "g (contains h)\n" +
                "i j (contains k)"
                |> parse
                |> analyze
                |> options 
                |> Seq.head =! ("k", "i") }
            test "demoinput" {
                let state = analyze (parse demoinput)
                options state |> Set =! Set [
                    "fish", "mxmxvkd"
                    "fish", "sqjhc"
                    "soy", "fvjkl"
                    "soy", "sqjhc" ]
            }
        ]
        testList "analyze" [
            test "demoinput" {
                let state = analyze (parse demoinput)
                state.["soy"]   =! Set ["sqjhc";"fvjkl"]
                state.["dairy"] =! Set ["mxmxvkd"] } ]
        testList "parsing" [
            test "demoinput" {
                let rules = parse demoinput
                rules.[0].Ingredients =!
                    Set [ "mxmxvkd";"kfcds";"sqjhc";"nhms" ]
                rules.[0].Allergenes =! Set [ "dairy";"fish" ]
                rules.[1].Allergenes =! Set [ "dairy" ] }
            test "a line" {
                let rule = parseLine "a b (contains c, d)"
                rule.Ingredients =! Set ["a";"b"]
                rule.Allergenes  =! Set ["c";"d"]
            }
        ]
    ]


