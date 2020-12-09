module Tests

open Swensen.Unquote
let utest = test
open Expecto
open Solution
open Demoinput


[<Tests>]
let allTests =
    testList "all" [
        testList "solve" [
            test "demoinput" { solve 5 demoinput =! 127L } ]

        testList "change preamble" [
            test "appendValue" {
                let p =
                    {   lowestIndex   = 2
                        highestIndex  = 4
                        indexedValues = Map [2,10L; 3,20L; 4,100L]
                        validPairs    = [   ValidPair.create 30L 2 3
                                            ValidPair.create 110L 2 4
                                            ValidPair.create 120L 3 4 ] }
                    |> Preamble.appendValue 900L
                p.lowestIndex   =! 2
                p.highestIndex  =! 5
                p.validPairs    =! [   ValidPair.create   30L  2 3
                                       ValidPair.create  110L  2 4
                                       ValidPair.create  120L  3 4
                                       ValidPair.create  910L  2 5
                                       ValidPair.create  920L  3 5
                                       ValidPair.create 1000L  4 5 ]
                p.indexedValues =! Map [2,10L; 3,20L; 4,100L; 5,900L] }
            test "removeFirst" {
                let p =
                    {   lowestIndex   = 2
                        highestIndex  = 4
                        indexedValues = Map [2,10L; 3,20L; 4,100L]
                        validPairs    = [   ValidPair.create 30L 2 3
                                            ValidPair.create 110L 2 4
                                            ValidPair.create 120L 3 4 ] }
                    |> Preamble.removeFirstValue
                p.lowestIndex   =! 3
                p.highestIndex  =! 4
                p.validPairs    =! [ ValidPair.create 120L 3 4 ]
                p.indexedValues =! Map [3, 20L; 4, 100L] } 
            test "cycle Preamble /w size 3" {
                let p =
                    {   lowestIndex   = 2
                        highestIndex  = 4
                        indexedValues = Map [2,10L; 3,20L; 4,100L]
                        validPairs    = [   ValidPair.create 30L 2 3
                                            ValidPair.create 110L 2 4
                                            ValidPair.create 120L 3 4 ] }
                    |> Preamble.cycle 1000L
                p.lowestIndex  =! 3
                p.highestIndex =! 5
                p.validPairs   =! [   ValidPair.create  120L 3 4
                                      ValidPair.create 1020L 3 5
                                      ValidPair.create 1100L 4 5 ] } ]

        testList "cycle Preamble /w size 2" [
            let myTest x = test (sprintf "value %d" x) {
                let p =
                    {   lowestIndex   = 5
                        highestIndex  = 6
                        indexedValues = Map [5,10L; 6,20L]
                        validPairs    = [ ValidPair.create 30L 5 6 ] }
                    |> Preamble.cycle x
                p.lowestIndex  =! 6
                p.highestIndex =! 7
                p.validPairs   =! [ ValidPair.create (x+20L) 6 7 ]
                p.indexedValues =! Map [6,20L; 7,x] }
            myTest 30L
            myTest 35L ]

        testList "create Preamble" [
            test "fromList" {
                let p = Preamble.fromList [0L;8L;15L]
                p.lowestIndex   =! 0
                p.highestIndex  =! 2
                p.indexedValues =! Map [0,0L; 1,8L; 2,15L] }
            test "fromTuple" {
                let p = Preamble.fromTuple (10L, 20L)
                p.lowestIndex   =! 0
                p.highestIndex  =! 1
                p.indexedValues =! Map [0,10L; 1,20L]
                p.validPairs    =! [ ValidPair.create 30L 0 1 ] }
            test "checkValue" {
                let p = Preamble.fromTuple (10L, 20L)
                Preamble.checkValue 30L p =! true
                Preamble.checkValue 29L p =! false } ]

        testList "parse" [
            test "demoinput" {
                parse demoinput =! [
                    35L;20L;15L;25L;47L;40L;62L;55L;65L;95L;102L;
                    117L;150L;182L;127L;219L;299L;277L;309L;576L] } ] ]
