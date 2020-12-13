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
            test "demoinput" { solve demoinput =! (17, -8, 25) } ]
        testList "actions" [
            let s o = { orientation = o; location = 0, 0 }
            test "forward" {
                action (s North) ('F', 1) =!
                    {   orientation = North
                        location = 0, 1 }
                action (s West) ('F', 1) =!
                    {   orientation = West
                        location = -1, 0 } }
            test "rotate" {
                rotate North 0 =! North
                rotate North 90 =! East
                rotate East 0 =! East
                rotate West 180 =! East }
            test "turns" {
                action (s East) ('R', 90) =! {
                    orientation = South
                    location    = 0, 0 }
                action (s North) ('R', 90) =! {
                    orientation = East
                    location    = 0, 0 }
                action (s East) ('R', 180) =! {
                    orientation = West
                    location    = 0, 0 } }
            test "simple moves" {
                action Ship.create ('N', 1) =! {
                    orientation = East
                    location    = 0, 1 }
                action Ship.create ('E', 1) =! {
                    orientation = East
                    location    = 1, 0 }
                action Ship.create ('W', 1) =! {
                    orientation = East
                    location    = -1, 0 }
                action Ship.create ('S', 1) =! {
                    orientation = East
                    location    = 0, -1 } } ]
        testList "parse" [
            test "demoinput" {
                parse demoinput =! [
                    'F', 10
                    'N', 3
                    'F', 7
                    'R', 90
                    'F', 11] }
        ]
    ]


