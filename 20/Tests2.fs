module Tests2

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Solution2
open Demoinput


let fitTilesH t1 t2 = t1.Edges.[Right] = t2.Edges.[Left]
let fitTilesV t1 t2 = t1.Edges.[Bottom] = t2.Edges.[Top]

[<Tests>]
let part2 =
    testList "part2" [
        testList "arrange" [
            test "demoinput" {
                let matrix = arrange (parse demoinput |> analyze)
                matrix
                |> Map.toList
                |> List.map snd
                |> List.distinct
                |> List.length =! 9 } ]
        testList "fitTiles" [
            test "vertical" {
                let t1 = { No = 0; Edges = Map [Bottom, "###"]}
                let t2 = { No = 0; Edges = Map [Top,  "###"]}
                let t3 = { No = 0; Edges = Map [Top,  "##."]}
                fitTilesV t1 t2 =! true
                fitTilesV t1 t3 =! false }
            test "horizontal" {
                let t1 = { No = 0; Edges = Map [Right, "###"]}
                let t2 = { No = 0; Edges = Map [Left,  "###"]}
                let t3 = { No = 0; Edges = Map [Left,  "##."]}
                fitTilesH t1 t2 =! true
                fitTilesH t1 t3 =! false }
        ]
    ]
