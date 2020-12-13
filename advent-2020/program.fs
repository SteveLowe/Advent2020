module advent2020.program

open System.Diagnostics

let time f =
    let sw = Stopwatch.StartNew()
    f()
    sw.Stop()
    printfn "Elapsed: %ims\n" sw.ElapsedMilliseconds

[<EntryPoint>]
let main _argv =
    time (day1.Solve)
    time (day2.Solve)
    time (day3.Solve)
    time (day4.Solve)
    time (day5.Solve)
    time (day6.Solve)
    time (day7.Solve)
    time (day8.Solve)
    time (day9.Solve)
    time (day10.Solve)
    time (day11.Solve)
    time (day12.Solve)
    0
