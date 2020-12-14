module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "all" [
        testList "solve" [ test "demoinput" { solve demoinput =! 165UL } ]
        testList "exec" [
            test "mask" {
                let s = { mask = ""; mem = Map.empty }
                exec s (Mask "ABC") =! { mask = "ABC"; mem = Map.empty } }
            test "mem" {
                let s = { mask = "XXXXXXXXXXXXXX1XXXX0X"; mem = Map.empty }
                let s' = exec s (Mem (100, 0))
                s'.mem.[100] =!  "000000000000001000000" } ]
        testList "parse" [
            test "regexMatch" {
                tryRegexMatch @"(\d) (\d)" "2 9" =! Some ["2"; "9"]
                tryRegexMatch @"(\d) (\d)" "A 9" =! None }
            test "demoinput" {
                parse demoinput =! [
                    Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                    Mem (8, 11)
                    Mem (7, 101)
                    Mem (8, 0) ] }
        ]
    ]


