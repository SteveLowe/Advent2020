[<AutoOpen>]
module advent2020.util

open System

let add a b = a + b
let is a b = a = b
let isNot a b = a <> b

let nullOrWhiteSpace s = String.IsNullOrWhiteSpace s
let notNullOrWhiteSpace = nullOrWhiteSpace >> not

let splitOnEmpty (arr: string array) =
    let rec loop (arr: string array, l) =
        match arr.Length with
        | 0 -> l |> List.rev |> List.toArray
        | _ ->
            let a =
                arr
                |> Array.skipWhile nullOrWhiteSpace
                |> Array.takeWhile notNullOrWhiteSpace

            let arr = arr |> Array.skipWhile nullOrWhiteSpace |> Array.skip a.Length
            let l = a :: l
            loop (arr, l)

    loop (arr, [])

// find first matching element in array
// returns a tuple of the element and and array with the remaining elements
let arrayTryPartition1 f (arr: 'a array) =
    match (arr |> Array.tryFind f) with
    | Some a -> (Some a, (arr |> Array.filter (isNot a)))
    | None -> (None, arr)
