module advent2020.day24

open System.IO

type Colour =
    | Black
    | White

type Direction =
    | NorthEast
    | East
    | SouthEast
    | SouthWest
    | West
    | NorthWest

type Coord = { X: int; Y: int; Z: int }
type Grid = Map<Coord, Colour>

let parseDirections (line: string) =
    let rec loop (chars: char array) index directions =
        if index >= chars.Length then
            directions
        else
            let direction, charsRead =
                match chars.[index] with
                | 'e' -> East, 1
                | 'w' -> West, 1
                | 's' ->
                    match chars.[index + 1] with
                    | 'e' -> SouthEast, 2
                    | 'w' -> SouthWest, 2
                    | c -> failwithf "s%c is not a valid direction" c
                | 'n' ->
                    match chars.[index + 1] with
                    | 'e' -> NorthEast, 2
                    | 'w' -> NorthWest, 2
                    | c -> failwithf "n%c is not a valid direction" c
                | c -> failwithf "%c is not a valid direction" c

            loop chars (index + charsRead) (direction :: directions)

    loop (line.ToCharArray()) 0 [] |> List.rev

let getInput file =
    File.ReadLines file
    |> Seq.map parseDirections
    |> Seq.toList

let followDirection direction coord =
    // using a cube coordinate system
    // https://www.redblobgames.com/grids/hexagons/
    match direction with
    | East ->
        { coord with
              X = coord.X + 1
              Y = coord.Y - 1 }
    | West ->
        { coord with
              X = coord.X - 1
              Y = coord.Y + 1 }

    | NorthEast ->
        { coord with
              X = coord.X + 1
              Z = coord.Z - 1 }
    | SouthWest ->
        { coord with
              X = coord.X - 1
              Z = coord.Z + 1 }

    | NorthWest ->
        { coord with
              Y = coord.Y + 1
              Z = coord.Z - 1 }
    | SouthEast ->
        { coord with
              Y = coord.Y - 1
              Z = coord.Z + 1 }

let followDirections grid start directions =
    let toggleColour grid coord =
        let colour =
            match grid |> Map.tryFind coord with
            | Some (colour) ->
                match colour with
                | White -> Black
                | Black -> White
            | None -> Black

        (grid |> Map.add coord colour), colour

    let rec loop grid coord directions =
        match directions with
        | [] -> toggleColour grid coord
        | _ ->
            let newCoord = followDirection directions.Head coord
            loop grid newCoord directions.Tail

    loop grid start directions

let flipTiles grid start directions =
    let rec loop grid toBlack toWhite directions =
        match directions with
        | [] -> grid, toBlack, toWhite
        | _ ->
            let grid, colour =
                followDirections grid start directions.Head

            let toBlack, toWhite =
                match colour with
                | Black -> toBlack + 1, toWhite
                | White -> toBlack, toWhite + 1

            loop grid toBlack toWhite directions.Tail

    loop grid 0 0 directions

let flipTilesDay grid days =
    let getTile grid coord =
        match grid |> Map.tryFind coord with
        | Some (colour) -> coord, colour
        | None -> coord, White

    let getAdjacentCoords coord =
        [| coord |> followDirection East
           coord |> followDirection SouthEast
           coord |> followDirection SouthWest
           coord |> followDirection West
           coord |> followDirection NorthWest
           coord |> followDirection NorthEast |]

    let countAdjacentTilesOf colour grid coord =
        coord
        |> getAdjacentCoords
        |> Array.map (getTile grid)
        |> Array.map snd
        |> Array.filter (eq colour)
        |> Array.length

    let getAllWhiteTiles grid =
        let addWhiteNeighbours grid coord colour =
            match colour with
            | White -> grid
            | Black ->
                coord
                |> getAdjacentCoords
                |> Array.map (getTile grid)
                |> Array.addToMap grid

        let whiteTiles =
            grid
            |> Map.fold addWhiteNeighbours grid
            |> Map.filter (veq White)

        whiteTiles

    let hasZeroOrTwoPlusBlackNeighbours grid coord _ =
        let blackNeighbours = coord |> countAdjacentTilesOf Black grid
        blackNeighbours = 0 || blackNeighbours > 2

    let hasTwoBlackNeighbours grid coord _ =
        let blackNeighbours = coord |> countAdjacentTilesOf Black grid
        blackNeighbours = 2

    let flipTo colour grid coord oldColour =
        if colour <> oldColour then grid |> Map.add coord colour else grid

    let rec loop (grid: Grid) days =
        match days with
        | 0 -> grid
        | _ ->
            // Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
            let flipToWhite =
                grid
                |> Map.filter (veq Black)
                |> Map.filter (hasZeroOrTwoPlusBlackNeighbours grid)

            // Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.
            let flipToBlack =
                grid
                |> getAllWhiteTiles
                |> Map.filter (hasTwoBlackNeighbours grid)

            let grid =
                flipToWhite |> Map.fold (flipTo White) grid

            let grid =
                flipToBlack |> Map.fold (flipTo Black) grid

            loop grid (days - 1)

    let grid = loop grid days

    let blackTiles =
        grid |> Map.filter (veq Black) |> Map.count

    blackTiles

let part1 input =
    let grid = Map.empty
    let start = { X = 0; Y = 0; Z = 0 }
    let _, toBlack, toWhite = flipTiles grid start input
    toBlack - toWhite

let part2 input =
    let grid = Map.empty
    let start = { X = 0; Y = 0; Z = 0 }
    let grid, _, _ = flipTiles grid start input
    flipTilesDay grid 100
