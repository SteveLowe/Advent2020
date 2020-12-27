module advent2020.day20

open System
open System.IO
open System.Text.RegularExpressions

type Edges =
    { Top: string
      Right: string
      Bottom: string
      Left: string }

type Tile = { Id: int64; Edges: Edges }
type TileOrientations = { Id: int64; Orientations: Edges list }

type Coord = { X: int; Y: int }
type Grid = Tile [,]

let parseTile (input: string array) =
    let (|IsTitle|_|) line =
        let m = Regex.Match(line, "^Tile ([0-9]+):$")
        if m.Success then Some m.Groups.[1].Value else None

    let title, tile = input.[0], input.[1..]

    let id =
        match title with
        | IsTitle id -> int64 id
        | _ -> failwithf "Tile is not valid - Missing Title:\n%A" input

    { Id = id
      Edges =
          { Top = (tile |> Array.head)
            Bottom = (tile |> Array.last)
            Left = tile |> Array.map firstChar |> Array.toString
            Right = tile |> Array.map lastChar |> Array.toString } }

let getTileOrientations (tile: Tile) =
    let rotate edges =
        { Top = edges.Left |> String.rev
          Right = edges.Top
          Bottom = edges.Right |> String.rev
          Left = edges.Bottom }

    let withFlipLeft edges =
        [ edges
          { Top = edges.Top |> String.rev
            Right = edges.Left
            Bottom = edges.Bottom |> String.rev
            Left = edges.Right } ]

    let withFlipTop edges =
        [ edges
          { Top = edges.Bottom
            Right = edges.Right |> String.rev
            Bottom = edges.Top
            Left = edges.Left |> String.rev } ]

    let getRotations edges =
        seq {
            yield edges
            let r1 = edges |> rotate
            yield r1
            let r2 = r1 |> rotate
            yield r2
            let r3 = r2 |> rotate
            yield r3
        }

    { Id = tile.Id
      Orientations =
          getRotations tile.Edges
          |> Seq.collect withFlipLeft
          |> Seq.collect withFlipTop
          |> Seq.distinct
          |> List.ofSeq }

let getInput file =
    File.ReadAllLines file
    |> Array.splitOnEmpty
    |> Array.map parseTile

let tileFitsTop top bottom = top.Bottom = bottom.Top
let tileFitsLeft left right = left.Right = right.Left

let populateGrid (tiles: TileOrientations array) =
    let gridSize = int (Math.Sqrt(float tiles.Length))
    let coordBounds = [| 0 .. (gridSize - 1) |]

    let validCoords =
        coordBounds
        |> Array.collect (fun x ->
            coordBounds
            |> Array.map (fun y -> { X = x; Y = y }))

    let findEmptyCell (grid: Map<Coord, Tile option>) =
        validCoords
        |> Array.pick (fun c ->
            match grid.[c] with
            | None -> Some c
            | _ -> None)

    let rec tryFillGrid (grid: Map<Coord, Tile option>) tiles tile =
        let whereFitsInCell coord tile =
            let leftMatches =
                match grid.TryFind { coord with X = coord.X - 1 } with
                | Some leftCell ->
                    tile.Orientations
                    |> List.filter (tileFitsLeft leftCell.Value.Edges)
                | None -> tile.Orientations

            let bothMatches =
                match grid.TryFind { coord with Y = coord.Y - 1 } with
                | Some upCell ->
                    leftMatches
                    |> List.filter (tileFitsTop upCell.Value.Edges)
                | None -> leftMatches

            bothMatches

        let coord = grid |> findEmptyCell
        let validOrientations = tile |> whereFitsInCell coord

        validOrientations
        |> List.tryPick (tryFillGridWith grid coord tiles tile.Id)

    and tryFillGridWith grid coord tiles tileId edges =
        let tile = { Id = tileId; Edges = edges }
//        printfn "Filling %i,%i with %i" coord.X coord.Y tileId
        // add tile to grid
        let grid = grid |> Map.add coord (Some tile)
        // remove tile from list
        let tiles =
            tiles |> Array.filter (fun t -> t.Id <> tile.Id)

        match tiles with
        | [||] -> Some grid
        | _ ->
            // recurse to fill next cell
            tiles |> Array.tryPick (tryFillGrid grid tiles)


    // get empty grid
    let grid =
        validCoords
        |> Array.map (fun c -> c, (None: Tile option))
        |> Map.ofArray

    let grid =
        tiles |> Array.pick (tryFillGrid grid tiles)

    let grid = grid |> Map.map (fun _ t -> t.Value)
    grid, (gridSize - 1)

let part1 input =
    let grid, maxIndex =
        input
        |> Array.map getTileOrientations
        |> populateGrid

    [ grid.[{ X = 0; Y = 0 }]
      grid.[{ X = 0; Y = maxIndex }]
      grid.[{ X = maxIndex; Y = 0 }]
      grid.[{ X = maxIndex; Y = maxIndex }] ]
    |> List.map (fun t -> t.Id)
    |> List.reduce (*)

let part2 input = 2