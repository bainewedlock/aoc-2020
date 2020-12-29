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
            test "demoinput" { solve demoinput =! 20899048083289L }]
        testList "analyze" [
            test "demoinput" {
                let result = analyze (parse demoinput)
                result.[1951] =! set [2311; 2729] } ]
        testList "parse" [
            test "verticalLine" {
                verticalLine 0 [ "123"; "456"; "789" ] =! "147"
                verticalLine 2 [ "123"; "456"; "789" ] =! "369" }
            test "demoinput" {
                let ts = parse demoinput
                ts.Count =! 9
                utest <@ ts.[2311].Edges.Contains "..##.#..#." @> // 1st line
                utest <@ ts.[2311].Edges.Contains "##..#....." |> not @> //2nd
                utest <@ ts.[2311].Edges.Contains "..###..###" @> // last
                utest <@ ts.[2311].Edges.Contains "...#.##..#" @> // right
                utest <@ ts.[2311].Edges.Contains ".#####..#." @> // left
            } ] ]


