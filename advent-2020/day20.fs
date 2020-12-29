module advent2020.day20

open System
open System.IO
open System.Text.RegularExpressions
open advent2020.util

type Edges =
    { Top: string
      Right: string
      Bottom: string
      Left: string }

type Tile =
    { Id: int
      Edges: Edges
      Picture: string array }

type TileOrientations =
    { Id: int
      Orientations: (Edges * string array) list }

type Coord = { X: int; Y: int }
type Grid = Tile [,]

let addEdges picture =
    { Top = (picture |> Array.head)
      Bottom = (picture |> Array.last)
      Left = picture |> Array.map firstChar |> Array.toString
      Right = picture |> Array.map lastChar |> Array.toString },
    picture

let parseTile (input: string array) =
    let (|IsTitle|_|) line =
        let m = Regex.Match(line, "^Tile ([0-9]+):$")
        if m.Success then Some m.Groups.[1].Value else None

    let title, tile = input.[0], input.[1..]

    let id =
        match title with
        | IsTitle id -> int id
        | _ -> failwithf "Tile is not valid - Missing Title:\n%A" input

    let edges, picture = tile |> addEdges

    { Id = id
      Edges = edges
      Picture = picture }

let getPictureOrientations picture =
    let rotate (picture: string array) =
        let getStringOfPos i _ =
            picture
            |> Array.map (fun s -> s.[i])
            |> Array.toString

        picture |> Array.mapi getStringOfPos

    let withFlipLeft picture =
        [ picture
          picture |> Array.map (String.rev) ]

    let withFlipTop picture = [ picture; picture |> Array.rev ]

    let getRotations picture =
        seq {
            yield picture
            let r1 = picture |> rotate
            yield r1
            let r2 = r1 |> rotate
            yield r2
            let r3 = r2 |> rotate
            yield r3
        }

    picture
    |> getRotations
    |> Seq.collect withFlipLeft
    |> Seq.collect withFlipTop
    |> Seq.distinct

let getTileOrientations (tile: Tile) =
    { Id = tile.Id
      Orientations =
          tile.Picture
          |> getPictureOrientations
          |> Seq.map addEdges
          |> List.ofSeq }

let getInput file =
    File.ReadAllLines file
    |> Array.splitOnEmpty
    |> Array.map parseTile

let tileFitsTop top (bottom, _) = top.Bottom = bottom.Top
let tileFitsLeft left (right, _) = left.Right = right.Left

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

    and tryFillGridWith grid coord tiles tileId (edges, picture) =
        let tile =
            { Id = tileId
              Edges = edges
              Picture = picture }
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
    |> List.map int64
    |> List.reduce (*)

let part2 input =
    let toPicture _ tile =
        Array.sub tile.Picture 1 (tile.Picture.Length - 2)
        |> Array.map (fun s -> s.Substring(1, s.Length - 2))

    let joinPicture (grid: Map<Coord, string array>) y2 picture coord =
        let tile = grid.[coord]
        picture + tile.[y2]

    let countChar c (s: string) =
        s.ToCharArray()
        |> Array.filter (eq c)
        |> Array.length

    let findTarget (target: string array) (picture: string array) =
        let lastCol = picture.[0].Length - target.[0].Length

        let target =
            target
            |> Array.map (fun l ->
                l.ToCharArray()
                |> Array.indexed
                |> Array.filter (sndIs '#'))

        let lineMatches targets (line: string) offset =
            targets
            |> Array.forall (fun (i, c) -> line.[offset + i] = c)

        let rec targetMatches picY offsetY possibleMatches =
            match possibleMatches, offsetY >= target.Length with
            | _, true -> Some possibleMatches
            | [], _ -> None
            | _ ->
                let newPossibleMatches =
                    possibleMatches
                    |> List.filter (lineMatches target.[offsetY] picture.[(picY + offsetY)])

                targetMatches picY (offsetY + 1) newPossibleMatches

        let rec loop y matches =
            if y >= (picture.Length - target.Length) then
                matches
            else
                match targetMatches y 0 [ 0 .. lastCol ] with
                | Some m ->
                    let newMatches =
                        m
                        |> List.map (fun x -> (x, y))
                        |> List.append matches

                    loop (y + 1) newMatches
                | None -> loop (y + 1) matches

        match loop 0 [] with
        | [] -> None
        | matches -> Some(picture, matches)

    let grid, maxIndex =
        input
        |> Array.map getTileOrientations
        |> populateGrid

    let picGrid = grid |> Map.map toPicture

    let picture =
        [| 0 .. maxIndex |]
        |> Array.collect (fun y -> [| 0 .. 7 |] |> Array.map (fun y2 -> y, y2))
        |> Array.map (fun (y, y2) ->
            [| 0 .. maxIndex |]
            |> Array.map (fun x -> { X = x; Y = y })
            |> Array.fold (joinPicture picGrid y2) "")

    let target =
        [| "                  # "
           "#    ##    ##    ###"
           " #  #  #  #  #  #   " |]

    let picture, matches =
        picture
        |> getPictureOrientations
        |> Seq.pick (findTarget target)

    let targetPixels = target |> Array.sumBy (countChar '#')
    let picturePixels = picture |> Array.sumBy (countChar '#')
    picturePixels - (matches.Length * targetPixels)
