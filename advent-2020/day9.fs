module advent2020.day9

open System.IO

let Solve () =
    let input =
        File.ReadLines "inputs/day9.txt"
        |> Seq.map int64
        |> Seq.toArray

    let findInvalidXmasNumber preamble (numbers: int64 array) =
        let isNumberValid i =
            let prevNums =
                numbers
                |> Array.skip (i - preamble)
                |> Array.take preamble

            prevNums |> anyTwoAddTo numbers.[i]

        let rec loop i =
            let isComplete = i >= numbers.Length

            match isComplete with
            | true -> failwithf "All numbers are valid"
            | _ ->
                match isNumberValid i with
                | false -> numbers.[i]
                | true -> loop (i + 1)

        loop preamble

    let answer1 = input |> findInvalidXmasNumber 25
    printfn "day8-part1:\n  Answer: %i" answer1

    let anyAddsTo target (numbers: int64 array) =
        let rec loop i =
            let isComplete = i >= numbers.Length

            match isComplete with
            | true -> None
            | _ ->
                let nums = numbers |> Array.take i
                let isTarget = target = (nums |> Array.sum)

                match isTarget with
                | true -> Some nums
                | _ -> loop (i + 1)

        loop 0

    let findWeaknessRange target (numbers: int64 array) =
        let rec loop i =
            let isComplete = i >= numbers.Length

            match isComplete with
            | true -> failwith "Failed to find result"
            | _ ->
                match anyAddsTo target (numbers |> Array.skip i) with
                | Some r -> r
                | None -> loop (i + 1)

        loop 0

    let answer2Range = input |> findWeaknessRange answer1

    let answer2 =
        (answer2Range |> Array.min)
        + (answer2Range |> Array.max)

    printfn "day8-part2:\n  Answer: %i" answer2
