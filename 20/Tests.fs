module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let part1 =
    testList "part1" [
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
                let ts = parse demoinput |> List.map (fun t -> t.No, t.Edges) |> Map
                ts.Count =! 9
                utest <@ ts.[2311].Contains "..##.#..#." @> // 1st line
                utest <@ ts.[2311].Contains "##..#....." |> not @> //2nd
                utest <@ ts.[2311].Contains "..###..###" @> // last
                utest <@ ts.[2311].Contains "...#.##..#" @> // right
                utest <@ ts.[2311].Contains ".#####..#." @> // left
            } ] ]


