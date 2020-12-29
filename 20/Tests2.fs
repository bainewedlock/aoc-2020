module Tests2

open Swensen.Unquote
let  utest = test
open Expecto

open Solution2
open Demoinput


//let fitTalesH t1 t2 = t1.Edges.[Right] = t2.Edges.[Left]
//let fitTalesV t1 t2 = t1.Edges.[Bottom] = t2.Edges.[Top]

let x : int[,] = Array2D.create 2 2 22

let arrange tiles =
    Array2D.init 2 2 (fun x y -> tiles |> List.item (y*2 + x))

//parse input

//[<Tests>]
//let part2 =
//    testList "part2" [
//        ptestList "arrange" [
//            test "simple 2x2 example without transformation" {
//                let t1 = { Edges = Map [Right, "###"; Bottom, "###"]}
//                let t2 = { Edges = Map [Left,  "###"]}
//                let t3 = { Edges = Map [Top,  "###"; Right, "###"]}
//                let t4 = { Edges = Map [Left,  "###"]}
//                let matrix = arrange [t1;t3;t2;t4]
//                matrix.[0,0] =! t1
//                matrix.[1,0] =! t2
//                matrix.[0,1] =! t3
//                matrix.[1,1] =! t4
//            }
//        ]
//        testList "fitTiles" [
//            test "vertical" {
//                let t1 = { Edges = Map [Bottom, "###"]}
//                let t2 = { Edges = Map [Top,  "###"]}
//                let t3 = { Edges = Map [Top,  "##."]}
//                fitTalesV t1 t2 =! true
//                fitTalesV t1 t3 =! false }
//            test "horizontal" {
//                let t1 = { Edges = Map [Right, "###"]}
//                let t2 = { Edges = Map [Left,  "###"]}
//                let t3 = { Edges = Map [Left,  "##."]}
//                fitTalesH t1 t2 =! true
//                fitTalesH t1 t3 =! false }
//        ]
//    ]
