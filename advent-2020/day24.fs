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

let followDirections grid start directions =
    let toggleColour grid coord =
        let colour =
            match grid |> Map.tryFind coord with
            | Some (colour) ->
                match colour with
                | White -> Black
                | Black -> White
            | None -> Black

        printfn "coord: %3i,%3i,%3i -> %A" coord.X coord.Y coord.Z colour
        (grid |> Map.add coord colour), colour

    let rec loop grid coord directions =
        match directions with
        | [] -> toggleColour grid coord
        | _ ->
            let newCoord =
                // using a cube coordinate system
                match directions.Head with
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

            loop grid newCoord directions.Tail

    loop grid start directions

let flipTiles grid start directions =
    let rec loop grid toBlack toWhite directions =
        match directions with
        | [] -> toBlack, toWhite
        | _ ->
            let grid, colour =
                followDirections grid start directions.Head

            let toBlack, toWhite =
                match colour with
                | Black -> toBlack + 1, toWhite
                | White -> toBlack, toWhite + 1

            loop grid toBlack toWhite directions.Tail

    loop grid 0 0 directions

let part1 input =
    let grid = Map.empty
    let start = { X = 0; Y = 0; Z = 0 }
    let toBlack, toWhite = flipTiles grid start input
    toBlack - toWhite

let part2 input = 2
