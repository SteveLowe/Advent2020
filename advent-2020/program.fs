module advent2020.program

open System.Diagnostics

let time f i =
    let sw = Stopwatch.StartNew()
    let answer = f i
    sw.Stop()
    printfn "Duration: %ims" sw.ElapsedMilliseconds
    answer


let solve day getInput f1 a1 f2 a2 =
    printfn $"day %i{day}:"
    let input = getInput ()

    let solvePart i f a =
        printfn $" part%i{i}:"
        let answer = time f input

        if a = answer then
            printfn $"  answer: %i{answer} ✅ Correct!"
            true
        else
            printfn $"  answer: %i{answer} ❌ Incorrect! expected %i{a}"
            false

    printfn ""
    match ((solvePart 1 f1 a1), (solvePart 2 f2 a2)) with
    | (true, true) -> ()
    | _ -> failwith $"day%i{day} failed"

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
    solve 18 day18.getInput day18.part1 23507031841020L day18.part2 218621700997826L
    solve 19 day19.getInput day19.part1 184 day19.part2 389

    0
