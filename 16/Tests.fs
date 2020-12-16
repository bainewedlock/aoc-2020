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
            test "demoinput" { solve demoinput =! 71 } ]
        testList "parse notes" [
            test "validField" {
                let r1 = Rule.create "r1" 1 3 5 7
                validField [r1] 4 =! false
                let r2 = Rule.create "r2" 1 4 5 7
                validField [r1; r2] 4 =! true }
            test "demoinput" {
                let notes = parse demoinput
                notes.nearbyTickets =! [7;3;47;40;4;50;55;2;20;38;6;12]
                notes.rules =! [
                    Rule.create "class" 1 3 5 7
                    Rule.create "row"   6 11 33 44
                    Rule.create "seat"  13 40 45 50 ] } ] ]


