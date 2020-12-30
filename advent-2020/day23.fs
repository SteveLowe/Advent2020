module advent2020.day23

open System.IO

let getInput file =
    File.ReadLines file
    |> Seq.head
    |> Seq.map string
    |> Seq.map int
    |> Seq.toArray

let playCups cups moves =
    let rec findDestinationIndex cups current =
        let dest =
            match current with
            | 0 -> cups |> Array.max
            | _ -> current - 1

        match cups |> Array.tryFindIndex (eq dest) with
        | Some i -> dest, (i + 1)
        | None -> findDestinationIndex cups dest

    let rec loop cups movesLeft =
        match movesLeft with
        | 0 -> cups
        | _ ->
            printfn "move %i cups %s" (moves - movesLeft + 1) (cups |> Array.take 8 |> Array.joinString " ")
            let current, cups = cups |> Array.popHead
            let picks, cups = ArrayLoop.subr cups 0 3
            let dest, afterDestIndex = findDestinationIndex cups current
            let beforeDest, afterDest = cups |> Array.splitAt afterDestIndex

            let nextCups =
                Array.concat [| [| current |]
                                beforeDest
                                picks
                                afterDest |]
                |> ArrayLoop.fromIndex 1

            loop nextCups (movesLeft - 1)

    loop cups moves

let part1 input =
    let finalCups = playCups input 100

    finalCups
    |> ArrayLoop.fromIndex (finalCups |> Array.findIndex (eq 1))
    |> Array.tail
    |> Array.toString
    |> int

let part2 input = 2
