module advent2020.day14

open System
open System.IO
open FParsec

type MaskBit =
    | Set
    | Clear
    | Other

type Instruction =
    | Mask of MaskBit array
    | Mem of (int64 * int64)

type A1State =
    { Values: Map<int64, int64>
      AndMask: int64
      OrMask: int64
      Mask: MaskBit array }

type A2State =
    { Values: Map<int64, int64>
      Mask: MaskBit array }

let Solve () =

    let instructions =
        let parseInstruction line =
            let peq = spaces >>. pchar '=' .>> spaces

            let parseMem =
                pstring "mem[" >>. pint64 .>> pchar ']' .>> peq
                .>>. pint64
                |>> Mem

            let parseMaskBit =
                let pset = pchar '1' >>% Set
                let pclear = pchar '0' >>% Clear
                let pignore = pchar 'X' >>% Other
                pset <|> pclear <|> pignore

            let parseMask =
                pstring "mask" >>. peq >>. parray 36 parseMaskBit
                |>> Mask

            let parseAnyInstruction = parseMask <|> parseMem

            match run parseAnyInstruction line with
            | Success (result, _, _) -> result
            | Failure (errorMsg, _, _) -> failwithf "Invalid instruction '%s' -> %s" line errorMsg

        File.ReadLines "inputs/day14.txt"
        |> Seq.map parseInstruction
        |> Seq.toArray

    let runA1Instruction (state: A1State) instruction =
        let getAndMask acc (i, mask) =
            match mask with
            | Clear -> acc |> clearBit64 i
            | _ -> acc

        let getOrMask acc (i, mask) =
            match mask with
            | Set -> acc |> setBit64 i
            | _ -> acc

        let updateMask mask =
            { state with
                  Mask = mask
                  OrMask =
                      mask
                      |> Array.rev
                      |> withi
                      |> Array.fold getOrMask 0L
                  AndMask =
                      mask
                      |> Array.rev
                      |> withi
                      |> Array.fold getAndMask Int64.MaxValue }

        match instruction with
        | Mask mask -> mask |> updateMask
        | Mem (addr, num) ->
            { state with
                  Values =
                      state.Values
                      |> Map.add addr ((num &&& state.AndMask) ||| state.OrMask) }

    let a1state =
        instructions
        |> Array.fold
            runA1Instruction
               { Values = Map.empty
                 OrMask = 0L
                 AndMask = Int64.MaxValue
                 Mask = repeat 36 Other |> Seq.toArray }

    let sumValues (acc: Int64) _ v = acc + v
    let answer1 = a1state.Values |> Map.fold sumValues 0L

    printfn "day14-part1:\n  Answer: %i" answer1

    let runA2Instruction (state: A2State) instruction =
        let applyBit l (i, b) =
            match b with
            | Clear -> l
            | Set -> l |> List.map (setBit64 i)
            | Other ->
                let l1 = l |> List.map (clearBit64 i)
                let l2 = l |> List.map (setBit64 i)
                l1 |> List.append l2

        let getAddrs mask addr =
            let addrs =
                mask
                |> Array.rev
                |> withi
                |> Array.fold applyBit [ addr ]
                |> List.toArray

            addrs

        let writeVal v (m: Map<'a, 'b>) k = m.Add(k, v)

        match instruction with
        | Mask mask -> { state with Mask = mask }
        | Mem (addr, num) ->
            { state with
                  Values =
                      addr
                      |> getAddrs state.Mask
                      |> Array.fold (writeVal num) state.Values }

    let a2State =
        instructions
        |> Array.fold
            runA2Instruction
               { Values = Map.empty
                 Mask = repeat 36 Other |> Seq.toArray }

    let answer2 = a2State.Values |> Map.fold sumValues 0L
    printfn "day14-part2:\n  Answer: %i" answer2
