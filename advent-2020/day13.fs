module advent2020.day13

open System.Diagnostics
open System.IO

type Bus = { Index: int64; ID: int64 }

let Solve () =
    let (arrival, busses) =
        let lines = File.ReadAllLines "inputs/day13.txt"
        if lines.Length <> 2 then failwithf "invalid input!"
        let arrival = lines.[0] |> int64

        let busses =
            lines.[1].Split(',')
            |> Array.mapi (fun i b -> (i, b))
            |> Array.filter (fun (_, b) -> b <> "x")
            |> Array.map (fun (i, b) -> { Index = int64 i; ID = int64 b })

        (arrival, busses)

    let findFirstBusAfter arrival (busses: Bus array) =
        let getNextTime (bus: Bus) =
            match arrival % bus.ID with
            | 0L -> 0L
            | o -> -o + bus.ID

        busses
        |> Array.map (fun b -> (b, b |> getNextTime))
        |> Array.minBy snd

    let (a1bus, a1wait) = findFirstBusAfter arrival busses
    let answer1 = a1bus.ID * a1wait

    printfn "day13-part1:\n  Answer: bus %i * wait %i -> %i" a1bus.ID a1wait answer1

    let findSequentialBusStartTime (busses: Bus array) =
        let verifyTime busses time =
            busses
            |> Array.forall (fun b -> (time + b.Index) % b.ID = 0L)

        let rec findSequentialStartTime busses time increment =
            match verifyTime busses time with
            | true ->
                (time,
                 busses
                 |> Array.map (fun b -> b.ID)
                 |> Array.reduce (*))
            | false -> findSequentialStartTime busses (time + increment) increment

        let rec loop busCount time increment =
            let isComplete = busCount >= busses.Length
            let lBusses = busses |> Array.take busCount
            let sw = Stopwatch.StartNew()

            match (isComplete, (findSequentialStartTime lBusses time increment)) with
            | (true, (time, _)) ->
                printfn "%i/%i solved in %A" busCount busses.Length sw.Elapsed
                time
            | (false, (time, increment)) ->
                printfn "%i/%i solved in %A" busCount busses.Length sw.Elapsed
                loop (busCount + 1) time increment


        let biggest2Bus =
            busses
            |> Array.take 2
            |> Array.map (fun b -> b.ID)
            |> Array.max

        loop 2 biggest2Bus 1L

    let answer2 = findSequentialBusStartTime busses
    printfn "day13-part2:\n  Answer: %i" answer2
