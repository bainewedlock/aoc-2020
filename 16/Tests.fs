module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput


[<Tests>]
let all =
    testList "all" [
        //testList "constraint propagation" {
        //    let notes = {
        //        rules = [ Rule.create "a" 1 2 3 4 ]
        //        nearbyTickets = [2] }
        //}
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
                notes.rules =! [
                    Rule.create "class" 1 3 5 7
                    Rule.create "row"   6 11 33 44
                    Rule.create "seat"  13 40 45 50 ] } ] ]


