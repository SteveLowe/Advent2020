module advent2020.day19

open System
open System.IO
open FParsec

type Rule =
    | Character of Char
    | RuleId of int
    | Pair of Rule * Rule
    | Triple of Rule * Rule * Rule
    | OneOf of Rule * Rule

type Rules = Map<int, Rule>

let parseRule line =
    // 0: 4 1 5
    // 1: 2 3 | 3 2
    // 4: "a"
    let pid = pint32 .>> skipChar ':' .>> spaces
    let pnum = pint32 .>> spaces

    let pCharRule =
        spaces >>. skipChar '"' >>. anyChar
        .>> skipChar '"'
        |>> Character

    let pRuleSingle = pnum |>> RuleId

    let pRulePair = tuple2 pRuleSingle pRuleSingle |>> Pair

    let pRuleTriple =
        tuple3 pRuleSingle pRuleSingle pRuleSingle
        |>> Triple

    let pSeqRule =
        attempt pRuleTriple
        <|> attempt pRulePair
        <|> pRuleSingle

    let pOneOfRule =
        pSeqRule .>> spaces .>> skipChar '|' .>> spaces
        .>>. pSeqRule
        |>> OneOf

    let pAnyRule =
        pCharRule <|> attempt pOneOfRule <|> pSeqRule

    let prule = pid .>>. pAnyRule

    match run (prule .>> eof) line with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf "failed to parse: %s\n%s" line errorMsg

let getInput (): Rules * string array =
    let parts =
        File.ReadAllLines "inputs/day19.txt"
        |> Array.splitOnEmpty

    if parts.Length <> 2
    then failwith "invalid input - too many blank lines"

    let rules =
        parts.[0] |> Array.map parseRule |> Map.ofArray

    (rules, parts.[1])

let matchesRule (rules: Rules) input =
    let rec loop rule (next: Rule option) (input: String) =
        // printfn "  Match %25s %A  next: (%A)" input rule next

        let getNext n =
            match next with
            | Some next -> Some(Pair(n, next))
            | None -> Some n

        if input = "" then
            false
        else
            match rule with
            | Character c ->
                match input.StartsWith c with
                | false ->
                    // printfn "        %25s %c <> %c" input c input.[0]
                    false
                | true ->
                    match next, input.Substring 1 with
                    | None, "" -> true
                    | None, _ -> false
                    | Some next, remainder -> loop next None remainder
            | RuleId id -> loop rules.[id] next input
            | Pair (a, b) -> loop a (getNext b) input
            | Triple (a, b, c) -> loop a (getNext (Pair(b, c))) input
            | OneOf (a, b) ->
                match loop a next input with
                | true -> true
                | false ->
                    // printfn "        %25s %A - %A Failed, trying %A" input rule a b
                    loop b next input

    match loop (rules.[0]) None input with
    | true ->
        // printfn "MATCHED!%25s" input
        (true, input)
    | _ -> (false, input)

let part1 (rules, input) =
    let result = input |> Array.map (matchesRule rules)
    result |> Array.filter fst |> Array.length

let part2 ((rules: Rules), input) =
    let rules =
        rules
            .Add(parseRule "8: 42 | 42 8")
            .Add(parseRule "11: 42 31 | 42 11 31")

    let result =
        input |> Array.Parallel.map (matchesRule rules)

    result |> Array.filter fst |> Array.length
