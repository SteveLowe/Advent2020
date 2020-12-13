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
    printfn "day11-part1:\n  FinalState: %O %i,%i\n  Answer: %i" a1Dir a1Pos.E a1Pos.N answer1
