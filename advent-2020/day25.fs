module advent2020.day25

open System.IO


let getInput file =
    let input =
        File.ReadLines file
        |> Seq.take 2
        |> Seq.map int64
        |> Seq.toArray

    input.[0], input.[1]

let encIter value subject = (value * subject) % 20201227L

let getLoopCount subject target =
    let rec loop value count =
        let key = encIter value subject

        match key = target with
        | true -> count
        | false -> loop key (count + 1)

    loop 1L 1

let encrypt loopCount subject =
    let rec loop value count =
        match count with
        | 0 -> value
        | _ ->
            let key = encIter value subject
            loop key (count - 1)

    loop 1L loopCount

let part1 (pub1, pub2) =
    let loop1 = getLoopCount 7L pub1
    encrypt loop1 pub2

let part2 (pub1, pub2) = 2
