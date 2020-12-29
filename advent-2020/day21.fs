module advent2020.day21

open System
open System.IO
open FParsec

type Food =
    { Ingredients: string array
      Allergens: string array }

let parseFood line =
    // mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
    let pIngredient = many1CharsTill letter spaces1
    let pIngredients = many1 pIngredient

    let pAllergen =
        many1CharsTill letter (pchar ',' <|> pchar ')')
        .>> spaces

    let pAllergens =
        between (pstring "(contains" .>> spaces) eof (many1 pAllergen)

    let parseLine =
        pIngredients .>>. attempt pAllergens
        |>> (fun (i, a) ->
            { Ingredients = i |> List.toArray
              Allergens = a |> List.toArray })

    match run parseLine line with
    | Success (result, _, _) -> result
    | Failure (errorMsg, _, _) -> failwithf "Failed to parse line:\n%s\n%s" line errorMsg

let getInput file =
    File.ReadLines file
    |> Seq.map parseFood
    |> Seq.toArray

let getIngredientForAllergens food allergens =
    let rec loop allergens allergensWithIngredient =
        match allergens with
        | [||] -> allergensWithIngredient
        | _ ->
            let allergen = allergens |> Array.head

            let possibleIngredient =
                food
                |> Array.filter (fun f -> f.Allergens |> Array.contains allergen)
                |> Array.map (fun f -> f.Ingredients)
                |> Array.reduce (fun acc ingredients -> acc |> Array.intersect ingredients)
                |> Array.except (allergensWithIngredient |> List.map snd)
                |> Array.distinct

            match possibleIngredient with
            | [| ingredient |] ->
                let nextAllergens = allergens |> Array.tail
                loop nextAllergens ((allergen, ingredient) :: allergensWithIngredient)
            | _ ->
                // put allergen at the back of the array, so we try again later
                let nextAllergens =
                    [| allergen |]
                    |> Array.append (allergens |> Array.tail)

                loop nextAllergens allergensWithIngredient

    loop allergens []

let part1 input =
    let allergens =
        input
        |> Array.collect (fun f -> f.Allergens)
        |> Array.distinct
        |> (getIngredientForAllergens input)
        |> List.toArray

    let allergenIngredients = allergens |> Array.map snd

    let nonAllergens =
        input
        |> Array.collect (fun f -> f.Ingredients)
        // can't use exclude as it removes duplicates
        |> Array.filter (fun i -> not (allergenIngredients |> Array.contains i))

    nonAllergens.Length


let part2 input =
    let allergens =
        input
        |> Array.collect (fun f -> f.Allergens)
        |> Array.distinct
        |> (getIngredientForAllergens input)
        |> List.toArray
        |> Array.sortBy fst
        
    let allergenIngredients = allergens |> Array.map snd
    String.Join(',', allergenIngredients)
