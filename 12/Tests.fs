module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        let swp x y = { location = 0, 0; waypoint = x, y }
        testList "solve2" [
            test "demoinput" { solve2 demoinput =! (214, -72, 286)} ]
        testList "move waypoint" [
            test "north" {
                part2Actions (swp 10 1) ('N', 3) =!
                    {   location = 0, 0
                        waypoint = 10, 4 } } ]
        testList "solve" [
            test "demoinput" { solve demoinput =! (17, -8, 25) } ]
        testList "actions" [
            test "forward" {
                part1Actions (swp 10 1) ('F', 10) =!
                    {   location = 100, 10
                        waypoint = 10, 1 }
                part1Actions (swp 2 1) ('F', 2) =!
                    {   location = 4, 2
                        waypoint = 2, 1 }
                part1Actions (swp -1 0) ('F', 1) =!
                    {   location = -1, 0
                        waypoint = -1, 0 } }
            test "turns" {
                part1Actions (swp 1 0) ('R', 90) =! {
                    waypoint    = 0, -1
                    location    = 0, 0 }
                part1Actions (swp 0 1) ('R', 90) =! {
                    location    = 0, 0 
                    waypoint    = 1, 0 }
                part1Actions (swp -1 0) ('R', 180) =! {
                    location    = 0, 0
                    waypoint    = 1, 0 } }
            test "simple moves" {
                let ship = { location = 0,0; waypoint = 0,0 }
                part1Actions ship ('N', 1) =! {
                    waypoint    = 0, 0
                    location    = 0, 1 }
                part1Actions ship ('E', 1) =! {
                    waypoint    = 0, 0
                    location    = 1, 0 }
                part1Actions ship ('W', 1) =! {
                    waypoint    = 0, 0
                    location    = -1, 0 }
                part1Actions ship ('S', 1) =! {
                    waypoint    = 0, 0
                    location    = 0, -1 } } ]
        testList "parse" [
            test "demoinput" {
                parse demoinput =! [
                    'F', 10
                    'N', 3
                    'F', 7
                    'R', 90
                    'F', 11] } ] ]


