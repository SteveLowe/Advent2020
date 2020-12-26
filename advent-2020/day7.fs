module advent2020.day7

open System.IO
open FParsec

type Bag =
    { Color: string
      AllowedBags: (int64 * string) array }

let Solve () =
    let parseBag (line: string) =
        let parseSubBag =
            spaces >>. pint64 .>> spaces
            .>>. charsTillString " bag" true 100
            .>> opt (pchar 's')
            .>> skipAnyOf [ ','; '.' ]

        let parseLine =
            spaces
            >>. charsTillString " bags contain" true 100
            .>> spaces
            .>>. many parseSubBag

        let (color, childBags) =
            match run parseLine line with
            | Success (result, _, _) -> result
            | Failure (errorMsg, _, _) -> failwithf "failed to parse line '%s': %s" line errorMsg

        let allowedBags = childBags |> List.toArray

        { Color = color
          AllowedBags = allowedBags }

    let findAllCanContainBag color (bags: Bag array) =
        let canContainAnyBag (colors: string list) (bag: Bag) =
            bag.AllowedBags
            |> Array.exists (fun (_, b) -> colors |> List.contains b)

        let rec loop colors matched remaining =
            let (m, remaining) =
                remaining
                |> Array.tryPartition1 (canContainAnyBag colors)

            match m with
            | None -> matched
            | Some b ->
                let matched = (b :: matched) |> List.distinct
                let colors = (b.Color :: colors) |> List.distinct
                loop colors matched remaining

        loop [ color ] [] bags

    let getAllChildBagCount color (bags: Bag array) =
        let isBag color (bag: Bag) = bag.Color = color
        let getChildBag (count, color) =
            let bag = bags |> Array.find (fun b -> b.Color = color)
            (count, bag)

        let rec getChildBagCount ((count: int64), (bag: Bag)) =
            let childCount =
                bag.AllowedBags
                |> Array.map getChildBag
                |> Array.map getChildBagCount
                |> Array.sum

            match count with
            | 0L -> childCount
            | _ -> count + (count * childCount)

        let bag = bags |> Array.find (isBag color)
        getChildBagCount (0L, bag)

    let bagRules =
        File.ReadLines "inputs/day7.txt"
        |> Seq.map parseBag
        |> Seq.toArray

    let answer1 =
        bagRules |> findAllCanContainBag "shiny gold"

    printfn "day7-part1:\n  Answer: %i" answer1.Length

    let answer2 =
        getAllChildBagCount "shiny gold" bagRules

    printfn "day7-part2:\n  Answer: %i" answer2
