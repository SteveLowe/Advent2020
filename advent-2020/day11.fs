module advent2020.day11

open System.IO

type Chair =
    | Floor
    | Empty
    | Occupied

let isOccupied c =
    match c with
    | Occupied -> true
    | _ -> false

let isNotFloor c =
    match c with
    | Occupied
    | Empty -> true
    | _ -> false

let Solve () =
    let tryGetChair (grid: Chair array array) x y dx dy =
        match grid |> Array.tryItem (y + dy) with
        | Some row -> row |> Array.tryItem (x + dx)
        | None -> None

    let tryGetNearestChair grid x y dx dy =
        let rec loop ldx ldy =
            match tryGetChair grid x y ldx ldy with
            | Some (c) when isNotFloor c -> Some(c)
            | None -> None
            | _ -> loop (ldx + dx) (ldy + dy)

        loop dx dy

    let progressRound (grid: Chair array array)
                      adjacentThreshold
                      (tryGetChair: Chair array array -> int -> int -> int -> int -> Chair option)
                      =
        let getAdjacentChairs x y =
            [| // left rop, middle, bottom
               tryGetChair grid x y -1 1
               tryGetChair grid x y -1 0
               tryGetChair grid x y -1 -1
               // middle top, bottom
               tryGetChair grid x y 0 1
               tryGetChair grid x y 0 -1
               // right rop, middle, bottom
               tryGetChair grid x y 1 1
               tryGetChair grid x y 1 0
               tryGetChair grid x y 1 -1 |]
            |> Array.choose id

        let getAdjacentOccupiedChairs x y =
            getAdjacentChairs x y
            |> Array.filter isOccupied
            |> Array.length

        let progressCell y x cell =
            match cell with
            | Empty ->
                // If a seat is empty and there are no occupied seats adjacent to it, the seat becomes occupied.
                match getAdjacentOccupiedChairs x y with
                | 0 -> Occupied
                | _ -> cell
            | Occupied ->
                // If a seat is occupied and $adjacentThreshold or more seats adjacent to it are also occupied, the seat becomes empty.
                match getAdjacentOccupiedChairs x y with
                | o when o >= adjacentThreshold -> Empty
                | _ -> cell
            | _ -> cell

        let progressRow y row = row |> Array.mapi (progressCell y)

        let newGrid = grid |> Array.mapi progressRow
        (newGrid <> grid, newGrid)

    let progressUntilStable adjacentThreshold tryGetChair grid =
        let rec loop chairGrid i =
            match progressRound chairGrid adjacentThreshold tryGetChair with
            | (false, _) -> (chairGrid, i)
            | (true, newGrid) -> loop newGrid (i + 1)

        loop grid 1

    let countOccupied grid =
        let countOccupiedRow row =
            row |> Array.filter isOccupied |> Array.length

        grid |> Array.map countOccupiedRow |> Array.sum

    let input =
        let parseGridCell c =
            match c with
            | '.' -> Floor
            | 'L' -> Empty
            | '#' -> Occupied
            | c -> failwithf "%c is not a valid grid cell" c

        let parseGridRow (row: string) =
            row.ToCharArray() |> Array.map parseGridCell

        File.ReadLines "inputs/day11.txt"
        |> Seq.map parseGridRow
        |> Seq.toArray

    let (a1grid, a1i) =
        input |> (progressUntilStable 4 tryGetChair)

    let answer1 = a1grid |> countOccupied
    printfn "day11-part1:\n  Rounds: %i\n  Answer: %i" a1i answer1

    let (a1grid, a1i) =
        input |> (progressUntilStable 5 tryGetNearestChair)

    let answer1 = a1grid |> countOccupied
    printfn "day11-part2:\n  Rounds: %i\n  Answer: %i" a1i answer1
