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

let playUntilWin deck1 deck2 =
    let withCards card1 card2 deck =
        let cards =
            if card1 > card2 then [| card1; card2 |] else [| card2; card1 |]

        cards |> Array.append deck

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
                    deck1, deck2 |> (withCards card1 card2)

            loop deck1 deck2

    loop deck1 deck2

let getScore (deck: Deck) =
    let addScore (acc: int) (index, card) = acc + ((index + 1) * card)

    deck
    |> Array.rev
    |> Array.indexed
    |> Array.fold addScore 0

let part1 (deck1, deck2) = playUntilWin deck1 deck2 |> getScore


let part2 input = 2
