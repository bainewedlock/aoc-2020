module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let moveTest =
    testList "moves" [
        test "solve part 1" { solve demoinput =! "67384529" }
        test "demoinput move 1" {
            move demoinput =! "289154673"
            move "574183926" =! "837419265"
        }
    ]

[<Tests>]
let gameTest =
    testList "tools" [
        ptest "rotateLeft" { rotateLeft 1 "abc" =! "bca" }
        test "calcTarget" { calcTarget "2" "891" =! "7"}
        test "pickup" { pickup "123456" =! ("123", "456") }
    ]

