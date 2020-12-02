module advent2020.day1

open System.IO

let input =
    File.ReadAllLines "inputs/day1.txt"
    |> Array.map int

let sumsTo2020 first second = first + second = 2020

let find2Sum2020 first =
    let secondOpt =
        input |> Array.tryFind (sumsTo2020 first)

    match secondOpt with
    | Some (second) -> Some(first, second)
    | None -> None

let findSums3To2020 first second =
    let thirdOpt =
        input
        |> Array.tryFind (sumsTo2020 (first + second))

    match thirdOpt with
    | Some (third) -> Some(second, third)
    | None -> None

let find3Sum2020 first =
    let answerOpt =
        input |> Array.tryPick (findSums3To2020 first)

    match answerOpt with
    | Some ((second, third)) -> Some(first, second, third)
    | None -> None

let solve =
    let (first, second) = input |> Array.pick find2Sum2020
    let answer = first * second
    printfn "day1-part1:\n  first: %i\n  second: %i\n  answer: %i\n" first second answer

    let (first, second, third) = input |> Array.pick find3Sum2020
    let answer = first * second * third
    printfn "day1-part2:\n  first: %i\n  second: %i\n  third: %i\n  answer: %i\n" first second third answer
