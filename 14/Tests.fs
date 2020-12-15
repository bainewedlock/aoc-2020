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
            test "demoinput2" { solve2 demoinput2 =! 208L } ]
        testList "step 2 helper functions" [
            test "replaceAt" {
                replaceAt 0 '!' "hallo" =! "!allo"
                replaceAt 4 '!' "hallo" =! "hall!" }
            test "calcAddresses 1" {
                calcAddresses "1" |> Set =! Set [1L] }
            test "calcAddresses X" {
                calcAddresses "X" |> Set =! Set [1L; 0L] }
            test "calcAddresses X1X" {
                calcAddresses "X1X" |> Set =! Set [2L; 3L; 6L; 7L] }
            test "maskAddress" {
                maskAddress "000X1001X" 42L =! "000X1101X" }
            test "setMultiple" {
                setMultiple 20L Map.empty [1L;2L] =! Map[
                    1L, 20L
                    2L, 20L] } ]
        testList "solve" [ test "demoinput" { solve demoinput =! 165L } ]
        testList "exec" [
            test "mask" {
                let s = { mask = ""; mem = Map.empty }
                exec s (Mask "ABC") =! { mask = "ABC"; mem = Map.empty } }
            test "mem" {
                let s = { mask = "XXXXXXXXXXXXXX1XXXX0X"; mem = Map.empty }
                let s' = exec s (Mem (100L, 0L))
                s'.mem.[100L] =! 64L } ]
        testList "parse" [
            test "regexMatch" {
                tryRegexMatch @"(\d) (\d)" "2 9" =! Some ["2"; "9"]
                tryRegexMatch @"(\d) (\d)" "A 9" =! None }
            test "demoinput" {
                parse demoinput =! [
                    Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
                    Mem (8L, 11L)
                    Mem (7L, 101L)
                    Mem (8L, 0L) ] } ] ]


