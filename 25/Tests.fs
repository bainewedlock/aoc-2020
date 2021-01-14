module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "solve" [ test "demoinput" { solve demoinput =! 14897079L } ]
        testList "tools" [
            test "calculate encryption key" {
                transform 17807724L |> Seq.item 8 =! 14897079L }
            test "determine loop size" {
                determineLoopSize 5764801L =! 8
                determineLoopSize 17807724L =! 11 }
            test "transform examples" {
                transform 7L |> Seq.item 8 =! 5764801L
                transform 7L |> Seq.item 11 =! 17807724L } ] ]


