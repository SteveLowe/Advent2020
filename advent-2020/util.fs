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

    let joinString (separator: string) (arr: 'T array) = String.Join(separator, arr)
    let toString (arr: 'T array) = arr |> joinString ""

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
        Set.intersect (Set.ofArray arr1) (Set.ofArray arr2)
        |> Set.toArray

    let popHead arr = arr |> Array.head, arr |> Array.tail

module List =
    let addToMap (map: Map<'a, 'b>) (lst: ('a * 'b) list) =
        let rec loop m lst =
            match lst |> List.tryHead with
            | None -> m
            | Some (a, b) -> loop (m |> Map.add a b) lst.Tail

        loop map lst

module ArrayLoop =
    /// Builds a new array that contains the given subrange specified by starting index and length.
    /// Will loop around the end of the Array
    let sub (arr: 'T array) start length =
        if length > arr.Length
        then failwithf "%i is longer than the array" length

        if start + length < arr.Length then
            Array.sub arr start length
        else
            arr
            |> Array.take (start - arr.Length + length)
            |> Array.append (arr |> Array.skip start)

    /// The same as sub, but also returns as a tuple, with the remainder of the ArrayLoop as the second element
    let subr (arr: 'T array) start length =
        let remainderStart = start + length % arr.Length
        let remainderLength = arr.Length - length
        sub arr start length, sub arr remainderStart remainderLength

    /// Builds a new array with all elements, starting at index
    /// Will loop around the end of the array
    let fromIndex index arr =
        let fromStart = arr |> Array.take index
        let fromIndex = arr |> Array.skip index
        fromStart |> Array.append fromIndex
