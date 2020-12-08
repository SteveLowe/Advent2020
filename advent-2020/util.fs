[<AutoOpen>]
module advent2020.util

open System

let add a b = a + b
let is a b = a = b
let isNot a b = a <> b

let notNullOrWhiteSpace (s: string) = not (String.IsNullOrWhiteSpace s)

let splitOnEmpty (arr: string array) =
    let mutable i = 0
    let mutable result: string array list = []

    while i < arr.Length do
        let a =
            arr
            |> Array.skip i
            |> Array.takeWhile notNullOrWhiteSpace

        i <- i + a.Length + 1
        if a.Length > 0 then result <- a :: result

    result |> List.rev |> List.toArray

// find first matching element in array
// returns a tuple of the element and and array with the remaining elements
let arrayTryPartition1 f (arr: 'a array) =
    match (arr |> Array.tryFind f) with
    | Some a -> (Some a, (arr |> Array.filter (isNot a)))
    | None -> (None, arr)
