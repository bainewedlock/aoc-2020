module Tests
open Swensen.Unquote
open Expecto
open Solution
open Demoinput

[<Tests>]
let solveTests =
    testList "step" [
        test "solve demoinput" {
            solve demoinput =! 5 }
        test "step democode" {
            let state2 = step democode State.initial
            state2.nextLine     =! 1
            state2.visitedLines =! Set [0]

            let state3 = step democode state2
            state3.nextLine     =! 2
            state3.acc          =! 1
            state3.visitedLines =! Set [0; 1]

            let state4 = step democode state3
            state4.nextLine     =! 6
            state4.acc          =! 1 } ]

[<Tests>]
let parseTests =
    testList "parse" [
        test "demoinput" {
            parse demoinput =! Map [ 0, Nop 0
                                     1, Acc 1
                                     2, Jmp 4
                                     3, Acc 3
                                     4, Jmp -3
                                     5, Acc -99
                                     6, Acc 1
                                     7, Jmp -4
                                     8, Acc 6 ] } ]
