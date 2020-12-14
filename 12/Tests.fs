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
            let s' x y = { Ship.create with waypoint = x, y }
            test "forward" {
                action (s' 2 1) ('F', 2) =!
                    {   location = 4, 2
                        waypoint = 2, 1 }
                action (s' -1 0) ('F', 1) =!
                    {   location = -1, 0
                        waypoint = -1, 0 } }
            test "turns" {
                action (s' 1 0) ('R', 90) =! {
                    waypoint    = 0, -1
                    location    = 0, 0 }
                action (s' 0 1) ('R', 90) =! {
                    location    = 0, 0 
                    waypoint    = 1, 0 }
                action (s' -1 0) ('R', 180) =! {
                    location    = 0, 0
                    waypoint    = 1, 0 }
            }
            test "simple moves" {
                action Ship.create ('N', 1) =! {
                    waypoint    = 1, 0
                    location    = 0, 1 }
                action Ship.create ('E', 1) =! {
                    waypoint    = 1, 0
                    location    = 1, 0 }
                action Ship.create ('W', 1) =! {
                    waypoint    = 1, 0
                    location    = -1, 0 }
                action Ship.create ('S', 1) =! {
                    waypoint    = 1, 0
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


