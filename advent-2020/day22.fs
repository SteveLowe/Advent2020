module advent2020.day22

open System.IO

type Deck = int array

let parseDeck input = input |> Array.tail |> Array.map int

let getInput file =
    let input =
        File.ReadAllLines file
        |> Array.splitOnEmpty
        |> Array.map parseDeck

    input.[0], input.[1]

let playCombat deck1 deck2 =
    let withCards card1 card2 deck = [| card1; card2 |] |> Array.append deck

    let rec loop deck1 deck2 =
        match deck1, deck2 with
        | _, [||] -> deck1
        | [||], _ -> deck2
        | _ ->
            let card1, deck1 = deck1 |> Array.popHead
            let card2, deck2 = deck2 |> Array.popHead

            let deck1, deck2 =
                if card1 > card2 then
                    deck1 |> (withCards card1 card2), deck2
                else
                    deck1, deck2 |> (withCards card2 card1)

            loop deck1 deck2

    loop deck1 deck2

let playRecursiveCombat deck1 deck2 =
    let withCards card1 card2 deck = [| card1; card2 |] |> Array.append deck

    let rec loop deck1 deck2 seenDecks1 seenDecks2 =
        match deck1, deck2 with
        | _, [||] -> 1, deck1
        | [||], _ -> 2, deck2
        | _ ->
            // if there was a previous round in this game that had exactly the same cards in the same order in the same players' decks,
            //  the game instantly ends in a win for player 1.
            match seenDecks1 |> List.contains deck1, seenDecks2 |> List.contains deck2 with
            | true, _
            | _, true -> 1, deck1
            | _ ->
                let seenDecks1 = deck1 :: seenDecks1
                let seenDecks2 = deck1 :: seenDecks2
                let card1, deck1 = deck1 |> Array.popHead
                let card2, deck2 = deck2 |> Array.popHead

                let player1WonRound =
                    if deck1.Length >= card1 && deck2.Length >= card2 then
                        // recurse to sub-game
                        let winner, _ =
                            loop (deck1 |> Array.take card1) (deck2 |> Array.take card2) [] []

                        winner = 1
                    else
                        card1 > card2

                let deck1, deck2 =
                    if player1WonRound then
                        deck1 |> (withCards card1 card2), deck2
                    else
                        deck1, deck2 |> (withCards card2 card1)

                loop deck1 deck2 seenDecks1 seenDecks2

    let _, deck = loop deck1 deck2 [] []
    deck

let getScore (deck: Deck) =
    let addScore (acc: int) (index, card) = acc + ((index + 1) * card)

    deck
    |> Array.rev
    |> Array.indexed
    |> Array.fold addScore 0

let part1 (deck1, deck2) = playCombat deck1 deck2 |> getScore


let part2 (deck1, deck2) =
    playRecursiveCombat deck1 deck2 |> getScore
