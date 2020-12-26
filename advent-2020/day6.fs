module advent2020.day6

open System
open System.IO

type GroupAnswers = { Answers: Set<Char> }

let Solve () =
    let parseAnswer setFunc (acc: GroupAnswers) (line: string) =
        let answers =
            line.ToCharArray()
            |> Set.ofArray
            |> setFunc acc.Answers

        { Answers = answers }

    let parseGroupAnyAnswers group =
        group
        |> Array.fold (parseAnswer Set.union) { Answers = Set.empty }

    let groups =
        File.ReadAllLines "inputs/day6.txt"
        |> Array.splitOnEmpty


    let answer1 =
        groups
        |> Array.map parseGroupAnyAnswers
        |> Array.map (fun a -> a.Answers.Count)
        |> Array.sum

    printfn "day6-part1:\n  Answer: %i" answer1

    let allAnswers =
        [| 1 .. 26 |]
        |> Array.map (fun i -> char (96 + i))
        |> Set.ofArray

    let parseGroupAllAnswer group =
        group
        |> Array.fold (parseAnswer Set.intersect) { Answers = allAnswers }

    let answer2 =
        groups
        |> Array.map parseGroupAllAnswer
        |> Array.map (fun a -> a.Answers.Count)
        |> Array.sum

    printfn "day6-part2:\n  Answer: %i" answer2
