module advent2020.day16

open System.IO
open FParsec

type Rule =
    { Name: string
      Rule1: (int * int)
      Rule2: (int * int) }

type Input =
    { Rules: Rule array
      Ticket: int array
      OtherTickets: int array array }

let Solve () =
    let input =
        let groups =
            File.ReadAllLines "inputs/day16.txt"
            |> Array.splitOnEmpty

        let parseRule line =
            //e.g. class: 1-3 or 5-7
            let parseName =
                anyChar |> manyCharsTill <| skipChar ':'
                .>> spaces

            let parseRule = pint32 .>> skipChar '-' .>>. pint32

            let parse2Rules =
                parseRule .>> spaces .>> pstring "or" .>> spaces
                .>>. parseRule

            let toRule (name, (r1, r2)) = { Name = name; Rule1 = r1; Rule2 = r2 }

            match run (parseName .>>. parse2Rules |>> toRule) line with
            | Success (result, _, _) -> result
            | Failure (errorMsg, _, _) -> failwithf "failed to parse Rule: %s" errorMsg

        let parseTicket (line: string) = line.Split ',' |> Array.map int

        { Rules = groups.[0] |> Array.map parseRule
          Ticket = groups.[1].[1] |> parseTicket
          OtherTickets =
              groups.[2]
              |> Array.skip 1
              |> Array.map parseTicket }

    let validateRule i (rule: Rule) =
        let (min1, max1) = rule.Rule1
        let (min2, max2) = rule.Rule2

        (i >= min1 && i <= max1)
        || (i >= min2 && i <= max2)

    let validateField rules field =
        (field, rules |> Array.exists (validateRule field))

    let otherTickets =
        input.OtherTickets
        |> Array.map (Array.map (validateField input.Rules))

    let answer1 =
        otherTickets
        |> Array.sumBy (Array.filter nsnd >> Array.map fst >> Array.sum)

    printfn "day16-part1:\n  Answer: %i" answer1

    let getFieldPositions rules (tickets: int array array) fieldCount =
        let ticketFollowsRule (rule: Rule) i (ticket: int array) = validateRule ticket.[i] rule

        let allTicketsFollowRule (rule: Rule) i =
            tickets |> Array.forall (ticketFollowsRule rule i)

        let tryFindPositions rule =
            match [| 0 .. (fieldCount - 1) |]
                  |> Array.filter (allTicketsFollowRule rule) with
            | arr when arr.Length = 0 -> None
            | arr -> Some(rule, arr)

        let possibleRulePositions = rules |> Array.choose tryFindPositions

        let rec loop rulesWithPos matchedPositions matchedRules =
            let excludeMatched (rule, posArr) =
                (rule, posArr |> Array.except matchedPositions)

            let hasOnePosition (rule, (posArr: int array)) =
                match posArr.Length with
                | 1 -> Some(rule, posArr |> Array.head)
                | _ -> None

            let notRule target (rule, _) = rule <> target

            let firstRuleWithOnePosition =
                rulesWithPos
                |> Array.map excludeMatched
                |> Array.tryPick hasOnePosition

            match firstRuleWithOnePosition with
            | None -> matchedRules |> List.rev |> List.toArray
            | Some ((rule, pos)) ->
                let remainingRules =
                    rulesWithPos |> Array.filter (notRule rule)

                loop remainingRules (pos :: matchedPositions) ((rule, pos) :: matchedRules)

        loop possibleRulePositions [] []

    let validOtherTickets =
        otherTickets
        |> Array.filter (Array.forall snd)
        |> Array.map (Array.map fst)

    let fieldPositions =
        getFieldPositions input.Rules validOtherTickets input.Ticket.Length

    let answer2 =
        let departureFields =
            fieldPositions
            |> Array.filter (fun (n, _) -> n.Name.StartsWith "departure")
            |> Array.map snd

        departureFields
        |> Array.map (fun i -> int64 input.Ticket.[i])
        |> Array.reduce (*)

    printfn "day16-part2:\n  Answer: %i" answer2
