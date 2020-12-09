module advent2020.day8

open System.IO
open FParsec

type Instructions =
    | Nop
    | Acc of Increment: int
    | Jmp of Jump: int

let Solve () =

    let parseInstruction (line: string) =
        let parseNop = pstring "nop" >>% Nop

        let parseAcc =
            pstring "acc" >>. spaces >>. pint32
            >>= fun i -> preturn (Acc i)

        let parseJmp =
            pstring "jmp" >>. spaces >>. pint32
            >>= fun i -> preturn (Jmp i)

        let parseAnyInstruction = parseNop <|> parseAcc <|> parseJmp

        match run parseAnyInstruction line with
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) -> failwithf "Invalid instruction '%s' -> %s" line errorMsg

    let findAccBeforeLoop (instructions: Instructions array) =
        let runInstruction i acc (seen: int list) =
            let seen = i :: seen

            match instructions.[i] with
            | Nop -> (i + 1, acc, seen)
            | Acc inc -> (i + 1, acc + inc, seen)
            | Jmp jmp -> (i + jmp, acc, seen)

        let rec loop (i, acc, seen) =
            let (i, acc, seen) = runInstruction i acc seen

            match (seen |> List.contains i) with
            | true -> acc
            | false -> loop (i, acc, seen)

        loop (0, 0, [])

    let instructions =
        File.ReadLines "inputs/day8.txt"
        |> Seq.map parseInstruction
        |> Seq.toArray

    let answer1 = instructions |> findAccBeforeLoop
    printfn "day8-part1:\n  Answer: %i" answer1
    
    let answer2 = 0
    printfn "day8-part2:\n  Answer: %i" answer2
