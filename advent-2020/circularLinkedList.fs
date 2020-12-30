module advent2020.circularLinkedList

open System.Diagnostics

[<DebuggerDisplay("{Value}, {next.Value}, {next.Next.Value}, {next.Next.Next.Value}, {next.Next.Next.Next.Value}")>]
type CircularLinkedListNode<'T>(value: 'T) as this =
    let mutable next = this
    let mutable previous = this
    member public this.Value = value

    member this.Previous
        with public get () = previous
        and internal set (value: CircularLinkedListNode<'T>) = previous <- value

    member this.Next
        with public get () = next
        and internal set (value: CircularLinkedListNode<'T>) = next <- value

module CircularLinkedList =
    let ofArray (arr: 'a array) =
        let foldCLL (prev: CircularLinkedListNode<'a>) cur =
            let item = CircularLinkedListNode cur
            item.Previous <- prev
            prev.Next <- item
            item

        match arr.Length with
        | 0 -> failwithf "Cannot convert empty array to %s" (nameof CircularLinkedListNode)
        | 1 -> CircularLinkedListNode arr.[0]
        | _ ->
            let first =
                arr |> Array.head |> CircularLinkedListNode

            let last =
                arr |> Seq.skip 1 |> Seq.fold foldCLL first

            last.Next <- first
            first.Previous <- last
            first

    let toArray (start: CircularLinkedListNode<'a>) =
        let rec loop node value =
            seq {
                match node = start with
                | true -> yield value
                | false ->
                    yield value
                    yield! loop node.Next node.Value
            }

        loop start.Next start.Value |> Seq.toArray

    let toMap (listNode: CircularLinkedListNode<'a>): Map<'a, CircularLinkedListNode<'a>> =
        let finalNode = listNode.Previous

        let rec loop node =
            seq {
                match node = finalNode with
                | true -> yield (node.Value, node)
                | false ->
                    yield (node.Value, node)
                    yield! loop node.Next
            }

        loop listNode |> Map.ofSeq


    let take count listItem =
        let rec loop count (node: CircularLinkedListNode<'a>) =
            seq {
                match count with
                | 1 -> yield node.Value
                | 0 -> yield! Seq.empty
                | c when c < 0 -> failwithf "Cannot take negative items"
                | _ ->
                    yield node.Value
                    yield! loop (count - 1) node.Next
            }

        loop count listItem

    let find (predicate: 'a -> bool) listItem =
        let rec loop (listItem: CircularLinkedListNode<'a>) =
            match predicate listItem.Value with
            | true -> listItem
            | false -> loop listItem.Next

        loop listItem

    let max (start: CircularLinkedListNode<'a>) =
        let rec loop node max =
            match node = start with
            | true -> max
            | false ->
                let max =
                    if max > node.Value then max else node.Value

                loop node.Next max

        loop start.Next start.Value

    let rec next count (listItem: CircularLinkedListNode<'a>) =
        match count with
        | 0 -> listItem
        | 1 -> listItem.Next
        | c when c < 0 -> failwithf "Cannot next negative times"
        | _ -> next (count - 1) listItem.Next

    /// Insert the array of values after the given item
    let insertAfter (listItem: CircularLinkedListNode<'a>) (arr: 'a array) =
        let left = listItem
        let right = listItem.Next
        let first = arr |> ofArray
        let last = first.Previous
        left.Next <- first
        first.Previous <- left
        right.Previous <- last
        last.Next <- right

    /// move N nodes from after source to destination
    let moveAfter (source: CircularLinkedListNode<'a>) count (dest: CircularLinkedListNode<'a>) =
        // cut out toMove
        let toMove = source.Next
        let toMoveEnd = toMove |> next (count - 1)
        let afterMove = toMoveEnd.Next
        source.Next <- afterMove
        afterMove.Previous <- source

        // stick toMove in after dest
        let afterDest = dest.Next
        toMove.Previous <- dest
        toMoveEnd.Next <- afterDest
        dest.Next <- toMove
        afterDest.Previous <- toMoveEnd

        ()
