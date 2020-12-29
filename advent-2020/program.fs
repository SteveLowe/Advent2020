module advent2020.program

open System.Diagnostics

let time f i =
    let sw = Stopwatch.StartNew()
    let answer = f i
    sw.Stop()
    printfn $"  duration: %i{sw.ElapsedMilliseconds}ms"
    answer


let inline solve day part getInput solver expected =
    printfn $"day %i{day} part%i{part}:"

    let answer =
        getInput $"inputs/day%i{day}.txt" |> time solver

    if answer = expected then
        printfn $"  answer: %i{answer} ✅ Correct!"
        printfn ""
    else
        printfn $"  answer: %i{answer} ❌ Incorrect! expected %i{expected}"
        failwith $"day%i{day} part%i{part} failed"

[<EntryPoint>]
let main _argv =
    time day1.Solve ()
    time day2.Solve ()
    time day3.Solve ()
    time day4.Solve ()
    time day5.Solve ()
    time day6.Solve ()
    time day7.Solve ()
    time day8.Solve ()
    time day9.Solve ()
    time day10.Solve ()
    time day11.Solve ()
    time day12.Solve ()
    time day13.Solve ()
    time day14.Solve ()
    time day15.Solve ()
    time day16.Solve ()
    time day17.Solve ()

    solve 18 1 day18.getInput day18.part1 23507031841020L
    solve 18 2 day18.getInput day18.part2 218621700997826L

    solve 19 1 day19.getInput day19.part1 184
    solve 19 2 day19.getInput day19.part2 389

    solve 20 1 day20.getInput day20.part1 19955159604613L
    solve 20 2 day20.getInput day20.part2 1639
    
    solve 21 1 day21.getInput day21.part1 1913
    solve 21 2 day21.getInput day21.part2 -1

    0
