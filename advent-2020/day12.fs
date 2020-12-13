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
        let followInstruction (heading, posN, posE) instruction =
            let move direction units =
                let (posN, posE) =
                    match direction with
                    | North -> (posN + units, posE)
                    | South -> (posN - units, posE)
                    | East -> (posN, posE + units)
                    | West -> (posN, posE - units)

                (heading, posN, posE)

            let turn direction units =
                let dirIndex =
                    Directions |> Array.findIndex (eq heading)

                let newDirIndex =
                    match direction with
                    | Right -> dirIndex + (units / 90)
                    | Left -> dirIndex - (units / 90) + Directions.Length

                (Directions.[newDirIndex % Directions.Length], posN, posE)

            match instruction with
            | Forward units -> move heading units
            | Direction (dir, units) -> move dir units
            | Turn (dir, units) -> turn dir units

        instructions
        |> Array.fold followInstruction (East, 0, 0)

    let input =
        File.ReadLines "inputs/day12.txt"
        |> Seq.map parseInstruction
        |> Seq.toArray

    let (a1Dir, a1N, a1E) = input |> followInstructions1
    let answer1 = abs a1N + abs a1E
    printfn "day11-part1:\n  FinalState: %O %i %i\n  Answer: %i" a1Dir a1N a1E answer1
