[<AutoOpen>]
module advent2020.util

open System

let inline add a b = a + b
let eq a b = a = b
let neq a b = a <> b

let isTrue (b: bool) = b
let isFalse (b: bool) = not b
let setBit64 i a = a ||| (1L <<< i)
let clearBit64 i a = a &&& ~~~(1L <<< i)

let nullOrWhiteSpace s = String.IsNullOrWhiteSpace s
let notNullOrWhiteSpace = nullOrWhiteSpace >> not

let nsnd (_, b) = not b

let sndIs x (_, b) = b = x 

let addsTo (target: int64) a b = a + b = target
let anyAddsTo target nums a = nums |> Array.exists (addsTo target a)

let anyTwoAddTo target nums =
    nums |> Array.exists (anyAddsTo target nums)

let firstChar (s: string) = s.ToCharArray() |> Array.head
let lastChar (s: string) = s.ToCharArray() |> Array.last

let getDiffs (numbers: int array) =
    let rec loop l i =
        match i >= numbers.Length with
        | true -> l |> List.rev |> List.toArray
        | _ ->
            let diff = numbers.[i] - numbers.[i - 1]
            let l = diff :: l
            loop l (i + 1)

    loop [] 1

let rec repeat n item =
    seq {
        if n > 0 then
            yield item
            yield! repeat (n - 1) item
    }

let argtup2 a b = (a, b)

module String =
    /// Reverse the characters in the string
    /// Will only work for ascii strings. unicode multi-byte chars will get mangled
    let rev (input: string) =
        String.Join("", input.ToCharArray() |> Array.rev)

module Seq =
    let withi seq = seq |> Seq.mapi argtup2

module Array =
    let withi arr = arr |> Array.mapi argtup2

    let toString (arr: char array) = String.Join("", arr)

    let addToMap (map: Map<'a, 'b>) (arr: ('a * 'b) array) =
        let rec loop m i =
            match arr |> Array.tryItem i with
            | None -> m
            | Some (a, b) -> loop (m |> Map.add a b) (i + 1)

        loop map 0

    let splitOnEmpty (arr: string array) =
        let rec loop (arr: string array, l) =
            match arr.Length with
            | 0 -> l |> List.rev |> List.toArray
            | _ ->
                let a =
                    arr
                    |> Array.skipWhile nullOrWhiteSpace
                    |> Array.takeWhile notNullOrWhiteSpace

                let arr =
                    arr
                    |> Array.skipWhile nullOrWhiteSpace
                    |> Array.skip a.Length

                let l = a :: l
                loop (arr, l)

        loop (arr, [])

    // find first matching element in array
    // returns a tuple of the element and and array with the remaining elements
    let tryPartition1 f (arr: 'a array) =
        match (arr |> Array.tryFind f) with
        | Some a -> (Some a, (arr |> Array.filter (neq a)))
        | None -> (None, arr)
        
    let intersect arr1 arr2 =
        Set.intersect (Set.ofArray arr1) (Set.ofArray arr2) |> Set.toArray

module List =
    let addToMap (map: Map<'a, 'b>) (lst: ('a * 'b) list) =
        let rec loop m lst =
            match lst |> List.tryHead with
            | None -> m
            | Some (a, b) -> loop (m |> Map.add a b) lst.Tail

        loop map lst
