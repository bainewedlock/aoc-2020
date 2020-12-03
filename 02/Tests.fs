module Tests

open Swensen.Unquote
open Expecto
open Solution

[<Tests>]
let tests = 
    testList "all" [
        test "parse line" {
            let input = "1-3 a: abcde"
            let pw = parseLine input
            pw.lo =! 1
            pw.hi =! 3
            pw.text =! "abcde"
            pw.letter =! "a" }
        test "solve demo 1" {
            let input = "1-3 a: abcde
                         1-3 b: cdefg
                         2-9 c: ccccccccc"
            solve input =! ["1-3 a: abcde", true;
                            "1-3 b: cdefg", false
                            "2-9 c: ccccccccc", true] }
        test "solve demo 2" {
            let input = "1-3 a: abcde
                         1-3 b: cdefg
                         2-9 c: ccccccccc"
            solve2 input =! ["1-3 a: abcde", true;
                             "1-3 b: cdefg", false
                             "2-9 c: ccccccccc", false] } ]

