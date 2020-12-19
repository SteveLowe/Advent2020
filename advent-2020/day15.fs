module advent2020.day15

open System.IO


let Solve () =
    let input =
        File.ReadLines "inputs/day15.txt"
        |> Seq.collect (fun s -> s.Split(','))
        |> Seq.map int
        |> Seq.toList

    let runMemGameTo target startNums =
        let rec loop (nums: Map<int, int>) num i =
            let newNum =
                match nums.TryFind num with
                | Some (ni) -> i - 1 - ni
                | None -> 0

            if i >= target
            then newNum
            else loop (nums.Add(num, i - 1)) newNum (i + 1)

        let nums =
            let toMap arr =
                arr
                |> List.mapi (fun i a -> (a, i + 1))
                |> Map.ofList
            startNums
            |> List.take (startNums.Length - 1)
            |> toMap

        loop nums (startNums |> List.last) (startNums.Length + 1)

    let answer1 = runMemGameTo 2020 input
    printfn "day15-part1:\n  Answer: %i" answer1

    let answer1 = runMemGameTo 30_000_000 input
    printfn "day15-part2:\n  Answer: %i" answer1
