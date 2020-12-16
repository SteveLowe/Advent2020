module advent2020.day14

open System
open System.IO
open FParsec

type MaskBit =
    | Set
    | Clear
    | Ignore

type Instruction =
    | Mask of MaskBit array
    | Mem of (int64 * int64)

type State =
    { Values: Map<int64, int64>
      AndMask: int64
      OrMask: int64
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
                let pignore = pchar 'X' >>% Ignore
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

    let runInstruction (state: State) instruction =
        let getAndMask acc (i, mask) =
            match mask with
            | Clear -> acc &&& ~~~(1L <<< (35 - i))
            | _ -> acc

        let getOrMask acc (i, mask) =
            match mask with
            | Set -> acc ||| (1L <<< (35 - i))
            | _ -> acc

        let updateMask mask =
            { state with
                  Mask = mask
                  OrMask = mask |> withi |> Array.fold getOrMask 0L
                  AndMask =
                      mask
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
            runInstruction
               { Values = Map.empty
                 OrMask = 0L
                 AndMask = Int64.MaxValue
                 Mask = repeat 36 Ignore |> Seq.toArray }

    let sumValues (acc: Int64) k v = acc + v
    let answer1 = a1state.Values |> Map.fold sumValues 0L

    printfn "day14-part1:\n  Answer: %i" answer1
