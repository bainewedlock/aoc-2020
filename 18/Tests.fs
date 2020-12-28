module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution

[<Tests>]
let all =
    testList "all" [
        testList "solve" [
            test "demoinput" {
                solve "2 * 3 + (4 * 5)
                       5 + (8 * 3 + 9 + 3 * 4 * 3)
                       5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
                       ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
                       =! string (26 + 437 + 12240 + 13632) } ]
        testList "eval" [
            test "demoinput 1" { simpleEval "1 + 2 * 3 + 4 * 5 + 6" =! "71" }
            test "demoinput 2" { eval "1 + (2 * 3) + (4 * (5 + 6))" =! "51" }
        ]
    ]


