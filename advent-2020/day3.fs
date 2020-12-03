module advent2020.day3

open System
open System.IO

type Tile =
    | Open
    | Tree

type TileSet = { Tiles: Tile array; Y: int }

type State = { X: int; Hits: int64; MoveX: int }

let Solve () =
    let parseTile char =
        match char with
        | '.' -> Open
        | '#' -> Tree
        | _ -> failwithf "%c is not a valid tile" char

    let parseLine i (line: string) =
        let tiles =
            line.ToCharArray() |> Array.map parseTile

        { Tiles = tiles; Y = i }

    let input =
        File.ReadAllLines "inputs/day3.txt"
        |> Array.mapi parseLine

    let moveToboggan (state: State) (tiles: TileSet) =
        let x =
            (state.X + state.MoveX) % tiles.Tiles.Length

        let hits =
            match tiles.Tiles.[x] with
            | Tree -> state.Hits + 1L
            | Open -> state.Hits

        { state with X = x; Hits = hits }

    let launchToboggan moveX moveY =
        let state =
            input
            |> Array.skip moveY
            |> Array.filter (fun line -> line.Y % moveY = 0)
            |> Array.fold moveToboggan { X = 0; Hits = 0L; MoveX = moveX }

        state.Hits

    let answer1 = (launchToboggan 3 1)
    printfn "day3-part1:\n  Answer: %i" answer1

    let answerSet2 =
        [| (launchToboggan 1 1)
           answer1
           (launchToboggan 5 1)
           (launchToboggan 7 1)
           (launchToboggan 1 2) |]

    let answer2 =
        answerSet2 |> Array.reduce (fun acc i -> acc * i)

    printfn "day3-part2:\n  Interim: %O\n  Answer: %i" (String.Join(',', answerSet2)) answer2
