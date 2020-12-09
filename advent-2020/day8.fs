module advent2020.day8

open System.IO
open FParsec

type Instruction =
    | Nop of int
    | Acc of int
    | Jmp of int

type ProcessResult =
    | Loop of int
    | Failed of string
    | Completed of int

let Solve () =
    let parseInstruction (line: string) =
        let pinstr s = pstring s >>. spaces >>. pint32
        let parseNop = pinstr "nop" |>> Nop
        let parseAcc = pinstr "acc" |>> Acc
        let parseJmp = pinstr "jmp" |>> Jmp
        let parseAnyInstruction = parseNop <|> parseAcc <|> parseJmp

        match run parseAnyInstruction line with
        | Success (result, _, _) -> result
        | Failure (errorMsg, _, _) -> failwithf "Invalid instruction '%s' -> %s" line errorMsg

    let runInstruction (instructions: Instruction array) i acc =
        match instructions.[i] with
        | Nop _ -> (i + 1, acc)
        | Acc inc -> (i + 1, acc + inc)
        | Jmp jmp -> (i + jmp, acc)

    let findAcc (instructions: Instruction array) =
        let lastInstruction = instructions.Length - 1

        let rec loop (i, acc, processed) =
            let isInvalid = i < 0
            let isCompleted = i > lastInstruction
            let alreadyProcessed = processed |> List.contains i

            match (isCompleted, isInvalid, alreadyProcessed) with
            | (true, _, _) -> Completed acc
            | (_, true, _) -> Failed "index out of bounds"
            | (_, _, true) -> Loop acc
            | _ ->
                let processed = i :: processed
                let (i, acc) = runInstruction instructions i acc
                loop (i, acc, processed)

        loop (0, 0, [])

    let findAccBeforeLoop instructions =
        match findAcc instructions with
        | Loop acc -> acc
        | Failed errorMsg -> failwithf "failed: %s" errorMsg
        | _ -> failwith "instructions did not loop!"

    let findAccAfterExecution (instructions: Instruction array) =
        let lastInstruction = instructions.Length - 1
        let isNopOrJmp instruction =
            match instruction with
            | Jmp _
            | Nop _ -> true
            | _ -> false

        let swapNopJmp instruction =
            match instruction with
            | Jmp i -> Nop i
            | Nop i -> Jmp i
            | _ -> failwithf "invalid instruction %O" instruction

        let swapNextInstruction (instructions: Instruction array) i =
            let offset =
                instructions
                |> Array.skip i
                |> Array.findIndex isNopOrJmp

            let i = i + offset
            let pre = instructions |> Array.take i
            let swp = [| instructions.[i] |> swapNopJmp |]
            let post = instructions |> Array.skip (i + 1)

            let swappedInstructions = Array.concat [| pre; swp; post |]
            (swappedInstructions, i)

        let rec loop i =
            let isCompleted = i >= lastInstruction
            match isCompleted with
            | true -> failwithf "Failed to find solution"
            | _ ->
                let (newInstructions, i) = swapNextInstruction instructions i
                match findAcc newInstructions with
                | Completed acc -> acc
                | Failed errorMsg -> failwith errorMsg
                | Loop _ -> loop (i + 1)
            
        loop 0

    let instructions =
        File.ReadLines "inputs/day8.txt"
        |> Seq.map parseInstruction
        |> Seq.toArray

    let answer1 = instructions |> findAccBeforeLoop
    printfn "day8-part1:\n  Answer: %i" answer1

    let answer2 = instructions |> findAccAfterExecution
    printfn "day8-part2:\n  Answer: %i" answer2
