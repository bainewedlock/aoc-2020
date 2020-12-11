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
            test "demoinput"  { solve2 demoinput  =! 8L }
            test "demoinput2" { solve2 demoinput2 =! 19208L } ]
        testList "analyze 1er chains" [
            test "demoinput" {
                parse demoinput |> Seq.toList |> analyze =! [2; 3; 4] }
            test "simple cases" {
                analyze [1; 3; 4; 6] =! [2]
                analyze [1; 3; 5] =! []
                analyze [0;1;2;4;5;10] =! [2;3] }
        ]
        testList "solve" [
            test "demoinput" { solve demoinput =! (7, 5) }
            test "demoinput2" { solve demoinput2 =! (22, 10) } ]
    ]


