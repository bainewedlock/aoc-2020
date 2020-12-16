module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "solve2" [
            test "demoinput2" { solve demoinput2 =! 0 }
            test "describeTicket" {
                preSolve demoinput2 =! Map [
                    "class", 12
                    "row",   11
                    "seat",  13 ] }
            test "small example" {
                Map [
                    "class", Set [0; 1]
                    "row",   Set [0]
                    "seat",  Set [2] ]
                |> searchSolution =! Some (Map [
                    "class", Set [1]
                    "row",   Set [0]
                    "seat",  Set [2] ]) }
            test "possible config" {
                Map [
                    "class", Set [0; 1]
                    "row",   Set [0]
                    "seat",  Set [2] ]
                |> possibleConfig =! true }
            test "constraint propagation" {
                let config = Map [
                    "class", Set [0; 1]
                    "row",   Set [0]
                    "seat",  Set [2] ]
                pick ("class", 0) config =! Map [
                    "class", Set [0]
                    "row",   Set []
                    "seat",  Set [2] ] } ]
        testList "validTickets" [
            test "demoinput" {
                let ts = parse demoinput |> validTickets
                ts =! [[7;1;14]; [7;3;47]] } ]
        testList "analyze" [
            test "demoinput" {
                let notes = parse demoinput
                analyze notes.rules (validTickets notes)
                    =! Map [
                        "class", Set [0; 1]
                        "row",   Set [0]
                        "seat",  Set [2] ] }
            test "single rule" {
                let rules = [
                    Rule.create "a" 1 5 0 0 
                    Rule.create "b" 0 0 10 20
                    Rule.create "c" 0 1 2 12 ]
                let tickets = [ [2; 12]; [1; 10] ]
                let result = analyze rules tickets
                result =! Map [
                    "a", Set [0]
                    "b", Set [1]
                    "c", Set [0; 1] ]} ]
        testList "solve" [
            test "demoinput" { solve demoinput =! 71 } ]
        testList "parse notes" [
            test "validField" {
                let r1 = Rule.create "r1" 1 3 5 7
                validField [r1] 4 =! false
                let r2 = Rule.create "r2" 1 4 5 7
                validField [r1; r2] 4 =! true }
            test "example input with space" {
                let notes = parse "
                    foo bar: 0-1 or 4-19
                    row: 0-5 or 8-19
                    seat: 0-13 or 16-19
                    
                    your ticket:
                    11,12,13
                    
                    nearby tickets:
                    3,9,18
                    15,1,5
                    5,14,9"
                notes.rules.Head =! Rule.create "foobar" 0 1 4 19 }
            test "demoinput" {
                let notes = parse demoinput
                notes.nearbyTickets =! [
                    [7;3;47]
                    [40;4;50]
                    [55;2;20]
                    [38;6;12]]
                notes.yourTicket =! [7;1;14]
                notes.rules =! [
                    Rule.create "class" 1 3 5 7
                    Rule.create "row"   6 11 33 44
                    Rule.create "seat"  13 40 45 50 ] } ] ]

