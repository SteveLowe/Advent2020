module advent2020.day23

open System.IO
open advent2020.circularLinkedList

let getInput file =
    File.ReadLines file
    |> Seq.head
    |> Seq.map string
    |> Seq.map int
    |> Seq.toArray
    |> CircularLinkedList.ofArray

let playCups cups moves =
    let maxValue = cups |> CircularLinkedList.max
    let cupDict = cups |> CircularLinkedList.toMap

    let rec findDestinationValue current picks =
        let dest =
            match current with
            | 1 -> maxValue
            | _ -> current - 1

        match picks |> Array.contains dest with
        | false -> dest
        | true -> findDestinationValue dest picks

    let findDestinationNode picks current =
        let dest = findDestinationValue current picks
        cupDict.[dest]

    let rec loop (cup: CircularLinkedListNode<int>) movesLeft =
        match movesLeft with
        | 0 -> cup
        | _ ->
            let picks =
                cup.Next
                |> CircularLinkedList.take 3
                |> Seq.toArray

            let destNode = findDestinationNode picks cup.Value
            CircularLinkedList.moveAfter cup 3 destNode

            loop cup.Next (movesLeft - 1)

    loop cups moves

let part1 (cups: CircularLinkedListNode<int>) =
    playCups cups 100 |> ignore

    cups
    |> CircularLinkedList.find (eq 1)
    |> CircularLinkedList.toArray
    |> Array.tail
    |> Array.toString
    |> int

let part2 (cups: CircularLinkedListNode<int>) =
    [| (CircularLinkedList.max cups + 1) .. 1000000 |]
    |> CircularLinkedList.insertAfter cups.Previous

    playCups cups 10000000 |> ignore

    let twoRightOfOne =
        cups
        |> CircularLinkedList.find (eq 1)
        |> CircularLinkedList.next 1
        |> CircularLinkedList.take 2
        |> Seq.toArray
        |> Array.map int64

    twoRightOfOne |> Array.reduce (*)
