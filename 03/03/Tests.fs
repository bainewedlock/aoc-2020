module Tests
open Swensen.Unquote
open Expecto
open Solution

let demoinput =
    "..##.......
     #...#...#..
     .#....#..#.
     ..#.#...#.#
     .#...##..#.
     ..#.##.....
     .#.#.#....#
     .#........#
     #.##...#...
     #...##....#
     .#..#...#.#"

[<Tests>]
let tests =
    testList "all" [
        test "solve demo 2" { solve2 demoinput =! 336u }
        test "solve demo" { solve demoinput =! 7 }
        test "findTrees single" {
            findTrees 3 (parse "....
                                ...#") =! Set [1] }
        test "findTrees" {
            let result = findTrees 3 (parse demoinput)
            result =! Set [2; 4; 5; 7; 8; 9; 10] } ]
