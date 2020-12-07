[<AutoOpen>]
module advent2020.util

open System

let add a b = a + b
let is a b = a = b
let (!=) a b = not (a = b)
    
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
