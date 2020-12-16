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
            test "demoinput" { solve demoinput =! 436 }
            test "spokenNumbers" {
                let sn = spokenNumbers [0;3;6] |> Seq.take 2020 |> Seq.toList
                sn |> List.take 10 =! [0;3;6;0;3;3;1;0;4;0]
                sn |> List.last    =! 436 } ]
        testList "history" [
            test "nextNumber after 0,2,6,0 is 3" {
                let h =
                    History.empty
                    |> History.add 0
                    |> History.add 2
                    |> History.add 6
                    |> History.add 0
                History.nextNumber h =! 3 }
            test "nextNumber after two zeros is 1" {
                let h =
                    History.empty
                    |> History.add 0
                    |> History.add 0
                History.nextNumber h =! 1 }
            test "nextNumber after single 0 is 0" {
                let h =
                    History.empty
                    |> History.add 0
                History.nextNumber h =! 0
            }
            test "lastNumberSpoken" {
                let h =
                    History.empty
                    |> History.add 5
                h.lastNumberSpoken =! 5 }
            test "SpokenMultipleTimes if added the third time" {
                let h = { History.empty with
                            log       = Map [100, SpokenMultipleTimes (5, 6) ] 
                            nextIndex = 8 }
                            |> History.add 100
                h.log       =! Map [100, SpokenMultipleTimes (6, 8)]
                h.nextIndex =! 9 }
            test "SpokenMultipleTimes if added the second time" {
                let h = { History.empty with
                            log       = Map [100, SpokenOnce 5] 
                            nextIndex = 10 }
                            |> History.add 100
                h.log       =! Map [100, SpokenMultipleTimes (5, 10)]
                h.nextIndex =! 11 }
            test "SpokenOnce if added the first time" {
                let h = History.empty |> History.add 1
                h.log =! Map [ 1, SpokenOnce 0]
                h.nextIndex =! 1
                let h' = h |> History.add 5
                h'.log =! Map [ 1, SpokenOnce 0; 5, SpokenOnce 1]
                h'.nextIndex =! 2 } ]
    ]


