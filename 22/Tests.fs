module Tests

open Swensen.Unquote
let  utest = test
open Expecto

open Solution
open Demoinput

[<Tests>]
let all =
    testList "Game" [
        test "solve part 1" { solve demoinput =! 306 }
        test "play to end" {
            let g = demoinput |> parse |> Game.create |> Game.playToEnd
            Game.decks g =! [[];[3;2;10;6;8;5;9;4;7;1]] }
        test "demoinput round #1-#2" {
            let g = Game.create (parse demoinput)
            let g1 = Game.play g
            Game.decks g1 =! [[2;6;3;1;9;5];[8;4;7;10]] } ]

[<Tests>]
let deckTests =
    testList "Deck" [
        test "putOneBelow" {
            let d = Deck.putOneBelow 5 (Deck.create [1;2;3])
            Deck.toList d =! [1;2;3;5] }
        test "drawOne" {
            let c, d = Deck.drawOne (Deck.create [1;2;3])
            c =! 1
            Deck.toList d =! [2;3] }
        test "create deck" {
            let d = Deck.create [1;2;3]
            Deck.toList d =! [1;2;3] }
        test "parse demoinput" {
            parse demoinput =! [[9;2;6;3;1];[5;8;4;7;10]] } ]

