namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Text.RegularExpressions

module Day14 =
    type Instruction =
        | Mask of string
        | Mem of uint64 * uint64

    let maskRegex = "^mask = ([X01]{36})$"
    let memRegex = "^mem\[(\d+)\] = (\d+)$"

    let parseFile (filename: string): seq<Instruction> =
        let parseInstruction (line: string): Instruction =
            let memMatch = Regex.Match(line, memRegex)
            let maskMatch = Regex.Match(line, maskRegex)

            if memMatch.Success then
                Mem(uint64 memMatch.Groups.[1].Value, uint64 memMatch.Groups.[2].Value)
            elif maskMatch.Success then
                let mask = maskMatch.Groups.[1].Value
                Mask mask
            else
                failwith "Invalid instruction"

        filename
        |> IO.File.ReadLines
        |> Seq.map parseInstruction


    let part1 (instructions: seq<Instruction>): uint64 =
        let inline applyMask (mask: string) number =
            let posMask =
                uint64
                <| Convert.ToInt64(mask.Replace("X", "0"), 2)

            let negMask =
                uint64
                <| Convert.ToInt64(mask.Replace("X", "1"), 2)

            (number &&& negMask) ||| posMask

        instructions
        |> Seq.fold (fun ((mask, mem): (string) * Map<uint64, uint64>) ->
            function
            | Mem (addr, value) -> (mask, Map.add addr (applyMask mask value) mem)
            | Mask newMask -> (newMask, mem)) ("X", Map.empty)
        |> snd
        |> Map.toSeq
        |> Seq.sumBy snd

    let part2 (instructions: seq<Instruction>): uint64 =
        let applyAddressMask (mask: string) address =
            mask.ToCharArray()
            |> Seq.ofArray
            |> Seq.rev
            |> Seq.mapi (fun idx char -> (idx, char))
            |> Seq.fold (fun addresses (idx, char) ->
                Seq.collect (fun addr ->
                    if char = 'X' then
                        Seq.ofList [ addr ||| (1UL <<< idx)
                                     addr &&& (~~~(1UL <<< idx)) ]
                    elif char = '1' then
                        Seq.singleton (addr ||| (1UL <<< idx))
                    else
                        Seq.singleton addr) addresses) (Seq.singleton address)

        instructions
        |> Seq.fold (fun ((mask, mem): (string) * Map<uint64, uint64>) ->
            function
            | Mem (addr, value) ->
                let newMem =
                    addr
                    |> applyAddressMask mask
                    |> Seq.fold (fun mem addr -> Map.add addr value mem) mem

                (mask, newMem)
            | Mask newMask -> (newMask, mem)) ("0", Map.empty)
        |> snd
        |> Map.toSeq
        |> Seq.sumBy snd

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
