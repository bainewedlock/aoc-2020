module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput
open Prettyprint

[<Tests>]
let all =
    testList "all" [
        testList "solve part 2" [
            test "demoinput" { solve2 demoinput =! 848 }]
        testList "solve part 1" [
            test "demoinput" { solve demoinput =! 112 } ]
        testList "step" [
            test "demoinput after 3 cycles" {
                parse demoinput
                |> step
                |> step
                |> step
                |> prettyprint 4 [1] =! [
                    "z=1"
                    "........."
                    "........."
                    "........."
                    "....#...."
                    ".....#..."
                    "..#......"
                    ".......##"
                    "...#...#."
                    "....#.#.."] }
            test "demoinput after 1 cycle" {
                step (parse demoinput)
                |> prettyprint 3 [-1]
                |> List.skip 5 =!  [
                    "...#..."
                    ".....#."
                    "....#.." ] } ]
        testList "pretty printing" [
            test "prettyprint" {
                parse demoinput
                |> prettyprint 2 [0;1] =! [
                    "z=0"
                    "....."
                    "....."
                    "...#."
                    "....#"
                    "..###"
                    ""
                    "z=1"
                    "....."
                    "....."
                    "....."
                    "....."
                    "....." ] } ]
        testList "parsing" [
            test "demoinput" {
                parse demoinput =! Set [
                   1, 0, 0, 0
                   2, 1, 0, 0
                   0, 2, 0, 0
                   1, 2, 0, 0
                   2, 2, 0, 0] } ] ]


