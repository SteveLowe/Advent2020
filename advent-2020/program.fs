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
        printfn $"  answer: %A{answer} ✅ Correct!"
        printfn ""
    else
        printfn $"  answer: %A{answer} ❌ Incorrect! expected %A{expected}"
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
    solve 21 2 day21.getInput day21.part2 "gpgrb,tjlz,gtjmd,spbxz,pfdkkzp,xcfpc,txzv,znqbr"

    solve 22 1 day22.getInput day22.part1 35299
    solve 22 2 day22.getInput day22.part2 33266

    solve 23 1 day23.getInput day23.part1 45286397
    solve 23 2 day23.getInput day23.part2 836763710L

    solve 24 1 day24.getInput day24.part1 434
    solve 24 2 day24.getInput day24.part2 3955
    
    solve 25 1 day25.getInput day25.part1 17032383L
    solve 25 2 day25.getInput day25.part2 2
    0
