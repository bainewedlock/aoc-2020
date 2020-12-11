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
            test "demoinput" { solve demoinput =! 37 }
        ]
        testList "parse" [
            test "demoinput" {
                let g = parse demoinput
                g.width  =! 10
                g.height =! 10
                g.data.[0, 0] =! 'L'
                g.data.[1, 0] =! '.'
                g.data.[1, 1] =! 'L' } ]
        testList "step" [
            test "adjacent" {
                let g = parse "..
                               .."
                utest <@ adjacent (0,0) g |> Set = Set [1,0; 0,1; 1,1] @>
                utest <@ adjacent (1,1) g |> Set = Set [1,0; 0,1; 0,0] @> }
            test "analyzeCell" {
                let g = parse "LLL
                               ###
                               ..."
                analyzeCell (0,0) g =! { content='L'; occupiedNeighbours=2 }
                analyzeCell (1,1) g =! { content='#'; occupiedNeighbours=2 }
                analyzeCell (1,0) g =! { content='L'; occupiedNeighbours=3 } }
            test "rule #1" {
                nextCellState { content='L'; occupiedNeighbours=0 } =! '#' }
            test "rule #2" {
                nextCellState { content='#'; occupiedNeighbours=4 } =! 'L' }
            test "rule #3 (default)" {
                nextCellState { content='L'; occupiedNeighbours=1 } =! 'L'
                nextCellState { content='#'; occupiedNeighbours=3 } =! '#' }
            test "demoinput" {
                demolines |> step |> Grid.print =! [
                    "#.##.##.##"
                    "#######.##"
                    "#.#.#..#.."
                    "####.##.##"
                    "#.##.##.##"
                    "#.#####.##"
                    "..#.#....."
                    "##########"
                    "#.######.#"
                    "#.#####.##" ]
                demolines |> step |> step |> Grid.print =! [
                    "#.LL.L#.##"
                    "#LLLLLL.L#"
                    "L.L.L..L.."
                    "#LLL.LL.L#"
                    "#.LL.LL.LL"
                    "#.LLLL#.##"
                    "..L.L....."
                    "#LLLLLLLL#"
                    "#.LLLLLL.L"
                    "#.#LLLL.##" ] } ] ]


