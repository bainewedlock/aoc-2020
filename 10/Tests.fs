module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

// possible combinations for 1 jolt diff:
// 
//          3 1 3
//         1 4 5 8
//         -> 1 variante
// 
//         3 1 1 3
//        1 4 5 6 9
//        -> v1: alles
//        -> v2: 5 raus
// 
//         3 1 1 1 3
//        1 4 5 6 7 10
//        -> v1: alles
//        -> v2: 5 raus
//        -> v3: 6 raus
//        -> v4: 5+6 raus


[<Tests>]
let all =
    testList "all" [
        testList "solve" [
            test "demoinput" { solve demoinput =! (7, 5) }
            test "demoinput2" { solve demoinput2 =! (22, 10) } ]
    ]


