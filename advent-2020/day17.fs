module advent2020.day17

open System.IO

type Cube =
    | Active
    | Inactive

let parseCube char =
    match char with
    | '#' -> Active
    | '.' -> Inactive
    | _ -> failwithf "%c is not a valid cube state" char

type Pos = { X: int; Y: int; Z: int; W: int }
type Grid = Map<Pos, Cube>

let Solve () =
    let startGrid =
        let parseGridChar y x char =
            ({ X = x; Y = y; Z = 0; W = 0 }, parseCube char)

        let parseGridLine (grid: Grid) (y, line: string) =
            line.ToCharArray()
            |> Array.mapi (parseGridChar y)
            |> Array.addToMap grid

        File.ReadLines "inputs/day17.txt"
        |> Seq.withi
        |> Seq.fold parseGridLine Map.empty

    let countActiveCubes grid =
        grid
        |> Map.filter (fun _k v ->
            match v with
            | Active -> true
            | _ -> false)
        |> Map.count

    let cycle getNeighbors grid =
        let getActiveNeighbors (pos: Pos) =
            let getWhereActive nPos =
                match grid |> (Map.tryFind nPos) with
                | Some (Active as cube) -> Some(cube)
                | _ -> None

            pos
            |> getNeighbors
            |> List.map getWhereActive
            |> List.choose id
            |> List.length

        let cycleCube pos cube =
            let activeNeighbors = pos |> getActiveNeighbors

            match cube with
            // If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active.
            | Active when activeNeighbors = 2 -> cube
            | Active when activeNeighbors = 3 -> cube
            // Otherwise, the cube becomes inactive.
            | Active -> Inactive
            // If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active.
            | Inactive when activeNeighbors = 3 -> Active
            // Otherwise, the cube remains inactive.
            | Inactive -> cube

        let addMissingCubes grid =
            let addMissing (grid: Grid) (pos: Pos) _cube =
                let getWhereNew (pos: Pos) =
                    match grid |> Map.tryFind pos with
                    | Some _ -> None
                    | None -> Some(pos, Inactive)

                pos
                |> getNeighbors
                |> List.map getWhereNew
                |> List.choose id
                |> List.addToMap grid

            grid |> Map.fold addMissing grid

        grid |> addMissingCubes |> Map.map cycleCube

    let getNeighbors3d (pos: Pos) =
        let getXYZ dx dy dz =
            { X = pos.X + dx
              Y = pos.Y + dy
              Z = pos.Z + dz
              W = 0 }

        let getXY dx dy =
            let notOrigin dx dy dz = dx <> 0 || dy <> 0 || dz <> 0

            [ -1 .. 1 ]
            |> List.filter (notOrigin dx dy)
            |> List.map (getXYZ dx dy)

        let getX dx = [ -1 .. 1 ] |> List.collect (getXY dx)

        [ -1 .. 1 ] |> List.collect getX


    let cycle3d = (cycle getNeighbors3d)

    let afterCycle3d6 =
        startGrid
        |> cycle3d
        |> cycle3d
        |> cycle3d
        |> cycle3d
        |> cycle3d
        |> cycle3d

    let answer1 = afterCycle3d6 |> countActiveCubes

    printfn "day17-part1:\n  Answer: %i" answer1

    let getNeighbors4d (pos: Pos) =
        let getXYZW dx dy dz dw =
            { X = pos.X + dx
              Y = pos.Y + dy
              Z = pos.Z + dz
              W = pos.W + dw }

        let getXYZ dx dy dz =
            let notOrigin dx dy dz dw = dx <> 0 || dy <> 0 || dz <> 0 || dw <> 0

            [ -1 .. 1 ]
            |> List.filter (notOrigin dx dy dz)
            |> List.map (getXYZW dx dy dz)

        let getXY dx dy =
            [ -1 .. 1 ] |> List.collect (getXYZ dx dy)

        let getX dx = [ -1 .. 1 ] |> List.collect (getXY dx)

        [ -1 .. 1 ] |> List.collect getX

    let cycle4d = (cycle getNeighbors4d)

    let afterCycle4d6 =
        startGrid
        |> cycle4d
        |> cycle4d
        |> cycle4d
        |> cycle4d
        |> cycle4d
        |> cycle4d

    let answer2 = afterCycle4d6 |> countActiveCubes
    printfn "day17-part2:\n  Answer: %i" answer2
