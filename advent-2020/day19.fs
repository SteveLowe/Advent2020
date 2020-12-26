module advent2020.day19

open System
open System.IO
open FParsec

type Rule =
    | Character of Char
    | RuleId of int
    | Pair of int * int
    | Triple of int * int * int
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

    let pRulePair = tuple2 pnum pnum |>> Pair

    let pRuleTriple = tuple3 pnum pnum pnum |>> Triple

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

    let rawRules =
        parts.[0] |> Array.map parseRule |> Map.ofArray

    let simplifyRules (rules: Rules) =
        let getRule id = rules.[id]

        let simplifyRule (id, rule) =
            let rec loop rule =
                match rule with
                | RuleId r -> getRule r
                | OneOf (a, b) -> OneOf((loop a), (loop b))
                | _ -> rule

            (id, loop rule)

        rules
        |> Map.toArray
        |> Array.map simplifyRule
        |> Rules

    //    let rules = simplifyRules rawRules
    (rawRules, parts.[1])

let matchesRule (rules: Rules) input =
    let getRule id = rules.[id]

    let rec loop rule (input: String) =
//        printfn "  Match %25s %A" input rule

        if input = "" then
            (false, input)
        else
            match rule with
            | Character c ->
                match input.StartsWith c with
                | true -> (true, (input.Substring 1))
                | false ->
//                    printfn "        %25s %c <> %c" input c input.[0]
                    (false, input)
            | RuleId id -> loop (getRule id) input
            | Pair (a, b) ->
                match loop (getRule a) input with
                | (false, _) -> (false, input)
                | (true, r) ->
//                    printfn "        %25s %A - %A succeeded, trying %A" input rule a b
                    let (bs, br) = loop (getRule b) r
                    (bs, br)
            | Triple (a, b, c) ->
                match loop (getRule a) input with
                | (false, _) -> (false, input)
                | (true, r) ->
//                    printfn "        %25s %A - %A succeeded, trying %A" input rule a b
                    let (bcs, bcr) = loop (Pair(b, c)) r
                    (bcs, bcr)
            | OneOf (a, b) ->
                match (loop a input) with
                | (true, r) -> (true, r)
                | (false, _) ->
//                    printfn "        %25s %A - %A Failed, trying %A" input rule a b
                    loop b input

    match loop (rules.[0]) input with
    | (true, "") -> (true, input)
    | (_, _) -> (false, input)

let part1 (rules, input) =
    let result = input |> Array.map (matchesRule rules)
    result |> Array.filter fst |> Array.length

let part2 (rules, input) = 2
