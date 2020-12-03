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
    0