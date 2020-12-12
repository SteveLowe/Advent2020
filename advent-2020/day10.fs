module advent2020.day10

open System.IO

type NumRepeat =
    | One of int
    | Three of int

let Solve () =
    let findDifferences (numbers: int array) =
        let diff1To3 cur prev d1 d2 d3 =
            match cur - prev with
            | 1 -> (d1 + 1, d2, d3)
            | 2 -> (d1, d2 + 1, d3)
            | 3 -> (d1, d2, d3 + 1)
            | d -> failwithf "diff of %i is not valid" d

        let rec loop i d1 d2 d3 =
            match i >= numbers.Length with
            | true -> (d1, d2, d3)
            | _ ->
                let (d1, d2, d3) =
                    diff1To3 numbers.[i] numbers.[i - 1] d1 d2 d3

                loop (i + 1) d1 d2 d3

        loop 1 0 0 0

    let input =
        let nums =
            File.ReadLines "inputs/day10.txt"
            |> Seq.map int
            |> Seq.toArray

        let deviceOutletValues = [| 0; (nums |> Array.max) + 3 |]

        nums
        // add the outlet and device joltages
        |> Array.append deviceOutletValues
        |> Array.sort

    let (a1d1, a1d2, a1d3) = input |> findDifferences
    let answer1 = a1d1 * a1d3
    printfn "day10-part1:\n  Differences: %i %i %i\n  Answer: %i" a1d1 a1d2 a1d3 answer1

    let getNumSequences (numbers: int array) =
        let updateList l cur acc =
            match cur with
            | 1 -> (One acc) :: l
            | 3 -> (Three acc) :: l
            | n -> failwithf "%i is not a valid diff" n

        let rec loop i prev acc (l: NumRepeat list) =
            match i >= numbers.Length with
            | true -> updateList l prev acc |> List.rev |> List.toArray
            | _ ->
                let cur = numbers.[i]

                let (acc, l) =
                    match cur = prev with
                    | true -> (acc + 1, l)
                    | false -> (1, updateList l prev acc)

                loop (i + 1) cur acc l

        loop 1 numbers.[0] 1 []

    let getSequencesVariations (diffSeqs: NumRepeat array) =
        let rec loop i acc =
            match i >= diffSeqs.Length with
            | true -> acc
            | _ ->
                let seq = diffSeqs.[i]
                let acc =
                    match seq with
                    | Three _ -> acc
                    | One i ->
                        match i with
                        | 1 -> acc
                        | 2 -> acc * 2L
                        | 3 -> acc * 4L
                        | 4 -> acc * 7L
                        | i -> failwithf "seq length of %i is not supported" i
                loop (i + 1) acc
        loop 0 1L

    // get an array of sequences of diffs
    let diffSeqs = input |> getDiffs |> getNumSequences
    let answer2 = getSequencesVariations diffSeqs
    printfn "day10-part2:\n  Inp: %A\n  Diffs: %A\n  Answer: %i" input diffSeqs answer2
