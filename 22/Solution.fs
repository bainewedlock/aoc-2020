module Solution

open System.Text.RegularExpressions

let parse (input:string) =
    let ms = Regex.Matches(input.Replace("\n", "+"), @"\+\d+")
    [ for m in ms do yield int m.Groups.[0].Value ]
    |> List.splitInto 2

type Card = int

module Deck =
    type T = private { cards : int list }
    let create (xs:Card seq) : T = { cards = Seq.toList xs }
    let toList (d:T) = d.cards
    let drawOne (d:T) : Card * T =
        match d.cards with
        | hd::tl -> hd, create tl
        | x      -> failwithf "unexpected %A" x
    let putOneBelow c { cards = cs } = create (List.append cs [c])

module Game =
    type T = private { decks : Map<int, Deck.T> }

    let create decks =
        { decks = decks |> List.map Deck.create |> List.indexed |> Map }

    let decks { decks = decks } =
        decks |> Map.toList |> List.sortBy fst |> List.map (snd>>Deck.toList)

    let play { decks = decks } =
        let players = decks |> Map.map (fun _ -> Deck.drawOne)
        let winner  = players |> Map.toSeq |> Seq.maxBy (snd >> fst) |> fst
        let playedCards = players |> Map.fold (fun cs _ (c,_) -> c::cs) []
        let winnerDeck =
            snd players.[winner]
            |> List.foldBack Deck.putOneBelow (playedCards |> List.sort)
        { decks =
            players
            |> Map.map (fun _ -> snd)
            |> Map.add winner winnerDeck}

    let rec playToEnd (g:T) =
        if decks g |> List.exists ((=)[]) then g
        else play g |> playToEnd

let solve =
    parse
    >> Game.create
    >> Game.playToEnd
    >> Game.decks
    >> List.concat
    >> List.rev
    >> List.mapi (fun i x -> (i+1) * x)
    >> List.sum
