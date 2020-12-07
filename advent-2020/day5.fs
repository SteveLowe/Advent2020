module advent2020.day5

open System.IO

type BoardingPass = { Row: int; Column: int; SeatId: int }

let Solve () =
    let parseBoardingPass (line: string) =
        let parseRowChar i c =
            match c with
            | 'F' -> 0
            | 'B' -> 1 <<< (6 - i)
            | _ -> failwithf "%c is not a valid row character" c

        let parseColumn i c =
            match c with
            | 'L' -> 0
            | 'R' -> 1 <<< (2 - i)
            | _ -> failwithf "%c is not a valid column character" c

        let chars = line.ToCharArray()

        let row =
            chars
            |> Array.take 7
            |> Array.mapi parseRowChar
            |> Array.sum

        let column =
            chars
            |> Array.skip 7
            |> Array.take 3
            |> Array.mapi parseColumn
            |> Array.sum

        { Row = row
          Column = column
          SeatId = ((row * 8) + column) }

    let findBeforeMissingEntry acc i = if i = (acc + 1) then i else acc

    let passes =
        File.ReadLines "inputs/day5.txt"
        |> Seq.map parseBoardingPass
        |> Seq.toArray

    let answer1 =
        passes |> Array.maxBy (fun p -> p.SeatId)

    printfn "day5-part1:\n  Answer: %i" answer1.SeatId

    let answer2 =
        passes
        |> Array.map (fun p -> p.SeatId)
        |> Array.sort
        |> Array.reduce findBeforeMissingEntry
        |> add 1


    printfn "day5-part2:\n  Answer: %i" answer2
