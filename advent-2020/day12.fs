module advent2020.day12

open System.IO

type Direction =
    | North
    | East
    | South
    | West

type Turn =
    | Left
    | Right

let Directions = [| North; East; South; West |]

type Instruction =
    | Forward of int
    | Turn of (Turn * int)
    | Direction of (Direction * int)

type Position = { E: int; N: int }

let parseInstruction (action: string): Instruction =
    let num = action.[1..] |> int

    match action.[0] with
    | 'F' -> Forward num
    | 'L' -> Turn(Left, num)
    | 'R' -> Turn(Right, num)
    | 'N' -> Direction(North, num)
    | 'E' -> Direction(East, num)
    | 'S' -> Direction(South, num)
    | 'W' -> Direction(West, num)
    | c -> failwithf "%c is not a valid action" c

let Solve () =
    let followInstructions1 (instructions: Instruction array) =
        let followInstruction (heading, pos) instruction =

            // move $direction by the given value.
            let move direction units =
                let pos =
                    match direction with
                    | North -> { pos with N = pos.N + units }
                    | South -> { pos with N = pos.N - units }
                    | East -> { pos with E = pos.E + units }
                    | West -> { pos with E = pos.E - units }

                (heading, pos)

            // turn left|right the given number of degrees.
            let turn direction degrees =
                let dirIndex =
                    Directions |> Array.findIndex (eq heading)

                let newDirIndex =
                    match direction with
                    | Right -> dirIndex + (degrees / 90)
                    | Left -> dirIndex - (degrees / 90) + Directions.Length

                (Directions.[newDirIndex % Directions.Length], pos)

            match instruction with
            | Forward units -> move heading units
            | Direction (dir, units) -> move dir units
            | Turn (dir, units) -> turn dir units

        instructions
        |> Array.fold followInstruction (East, { E = 0; N = 0 })

    let input =
        File.ReadLines "inputs/day12.txt"
        |> Seq.map parseInstruction
        |> Seq.toArray

    let (a1Dir, a1Pos) = input |> followInstructions1
    let answer1 = abs a1Pos.E + abs a1Pos.N
    printfn "day12-part1:\n  FinalState: %O %i,%i\n  Answer: %i" a1Dir a1Pos.E a1Pos.N answer1

    let followInstructions2 (instructions: Instruction array) =
        let followInstruction (pos, wp) instruction =

            // move forward to the waypoint a number of times equal to the given value.
            let move units =
                ({ pos with
                       E = pos.E + (wp.E * units)
                       N = pos.N + (wp.N * units) },
                 wp)

            // move the waypoint $direction by the given value.
            let moveWP direction units =
                let (wp) =
                    match direction with
                    | North -> { wp with N = wp.N + units }
                    | South -> { wp with N = wp.N - units }
                    | East -> { wp with E = wp.E + units }
                    | West -> { wp with E = wp.E - units }

                (pos, wp)

            // rotate the waypoint around the ship left|right by the given number of degrees.
            let turnWP direction degrees =
                let units = (degrees / 90) % 4 |> int

                let wp =
                    match (direction, units) with
                    | (_, 2) -> { wp with E = -wp.E; N = -wp.N }
                    | (_, 0) -> wp
                    | (Right, 3)
                    | (Left, 1) -> { wp with E = -wp.N; N = wp.E }
                    | (Left, 3)
                    | (Right, 1) -> { wp with E = wp.N; N = -wp.E }
                    | (_, _) -> failwithf "invalid turnWP %O %i" direction units

                (pos, wp)

            match instruction with
            | Forward units -> move units
            | Direction (dir, units) -> moveWP dir units
            | Turn (dir, units) -> turnWP dir units

        instructions
        |> Array.fold followInstruction ({ E = 0; N = 0 }, { E = 10; N = 1 })

    let (a2Pos, a2wp) = input |> followInstructions2
    let answer2 = abs a2Pos.E + abs a2Pos.N
    printfn "day12-part2:\n  Position: %i,%i\n  Waypoint: %i,%i\n  Answer: %i" a2Pos.E a2Pos.N a2wp.E a2wp.N answer2
