module Tests

open Swensen.Unquote
let utest = test

open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList
        "all"
        [ testList "solve2" [ test "demoinput" { solve2 demoinput =! 2208 } ]
          testList
              "step"
              [ test "steps" {
                  parseAndWalk demoinput
                  |> steps
                  |> Seq.take 5
                  |> Seq.toList
                  |> List.map (Set.count)
                  =! [ 10; 15; 12; 25; 14 ]
                }
                test "demoinput day #1" {
                    let day0 = parseAndWalk demoinput
                    day0 |> Set.count =! 10
                    day0 |> step |> Set.count =! 15
                    day0 |> step |> step |> Set.count =! 12
                    day0 |> step |> step |> step |> Set.count =! 25

                    day0 |> step |> step |> step |> step |> Set.count
                    =! 14
                } ]
          testList
              "neighbours"
              [ test "from center" {
                  neighbours (0, 0, 0) |> Set
                  =! Set(offset |> Seq.map (fun k -> k.Value))
                } ]
          testList "solve" [ test "demoinput" { solve demoinput =! 10 } ]
          testList
              "walking"
              [ test "adjacent to reference" {
                  let path = parse "esew" |> List.exactlyOne
                  walk path =! (0, 1, -1)
                }
                test "back to reference" {
                    let path = parse "nwwswee" |> List.exactlyOne
                    walk path =! (0, 0, 0)
                } ]
          testList
              "tools"
              [ test "convert direction to offset" {
                  offset.Item "e" =! (1, 0, -1) } ]
          testList
              "parsing"
              [ test "demoinput" {
                  let ps = parse demoinput
                  ps.Length =! 20
                }
                test "a line" { parseLine "esew" =! [ "e"; "se"; "w" ] } ] ]
