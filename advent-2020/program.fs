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
    0
