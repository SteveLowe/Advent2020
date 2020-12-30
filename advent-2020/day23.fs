module advent2020.day23

open System.IO

let getInput file =
    File.ReadLines file
    |> Seq.head
    |> Seq.map string
    |> Seq.map int
    |> Seq.toArray

let playCups cups moves =
    let rec findDestinationIndex cups picks current =
        let dest =
            match current with
            | 0 -> cups |> Array.max
            | _ -> current - 1

        match picks |> Array.contains dest with
        | true -> findDestinationIndex cups picks dest
        | _ ->
            match cups |> Array.tryFindIndex (eq dest) with
            | Some i -> i
            | None -> findDestinationIndex cups picks dest

    let rec loop cups movesLeft =
        match movesLeft with
        | 0 -> cups
        | _ ->
            // printfn "move %i cups %s" (moves - movesLeft + 1) (cups |> Array.take 8 |> Array.joinString " ")
            let current = cups |> Array.head
            let picks = ArrayLoop.sub cups 1 3
            let destIndex = findDestinationIndex cups picks current

            let nextCups = Array.create cups.Length 0
            nextCups.[cups.Length - 1] <- current
            nextCups.[0] <- cups.[4]
            // after picks to dest
            Array.blit cups 5 nextCups 1 (destIndex - 4)
            // picks
            Array.blit picks 0 nextCups (destIndex - 4 + 1) 3
            // after dest to end
            Array.blit cups (destIndex + 1) nextCups destIndex (cups.Length - destIndex - 1)

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
