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
            test "demoinput" {
                solve demoinput =! "59 * 5 = 295"
            }
        ]
        testList "parse" [
            test "demoinput" {
                parse demoinput =!  (939, [7; 13; 59; 31; 19]) }
        ]
    ]


