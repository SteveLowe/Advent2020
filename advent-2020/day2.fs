module advent2020.day2

open System
open System.IO

type PasswordPolicy = { Min: int; Max: int; Char: Char }

type PasswordEntry =
    { Policy: PasswordPolicy
      Password: string }

let Solve () =
    let parseInt (line: string) =
        let notNumber char = not (Char.IsNumber char)

        let firstNonNumberIndex =
            line.ToCharArray() |> Array.tryFindIndex notNumber

        match firstNonNumberIndex with
        | Some (index) ->
            let num = line.[0..index - 1] |> int
            (num, line.[index..])
        | None -> (0, line)

    let parsePolicy line =
        let (min, line) = parseInt line
        let (max, line) = parseInt line.[1..]
        let char = line.[1]
        { Min = min; Max = max; Char = char }

    let parseEntry (line: string) =
        let parts = line.Split(':', 2)
        if parts.Length <> 2 then failwith "Invalid input"

        let policy = parts.[0] |> parsePolicy
        let password = parts.[1].Trim()
        { Policy = policy; Password = password }

    let passwordIsValid1 (entry: PasswordEntry) =
        let policy = entry.Policy

        let charCount =
            entry.Password.ToCharArray()
            |> Array.filter (eq policy.Char)
            |> Array.length

        charCount >= policy.Min && charCount <= policy.Max

    let passwordIsValid2 (entry: PasswordEntry) =
        let policy = entry.Policy

        let charMatches index =
            match entry.Password.Length with
            | l when l < index -> false
            | _ -> entry.Password.[index] = policy.Char

        charMatches (policy.Min - 1)
        <> charMatches (policy.Max - 1)

    let input =
        File.ReadAllLines "inputs/day2.txt"
        |> Array.map parseEntry

    let answer1 =
        input
        |> Array.filter passwordIsValid1
        |> Array.length

    printfn "day2-part1:\n  Answer: %i" answer1

    let answer2 =
        input
        |> Array.filter passwordIsValid2
        |> Array.length

    printfn "day2-part2:\n  Answer: %i" answer2
