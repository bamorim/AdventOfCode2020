namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day25 =
    let parseFile (filename: string): uint64 * uint64 =
        filename
        |> IO.File.ReadLines
        |> Seq.map uint64
        |> Seq.truncate 2
        |> List.ofSeq
        |> (function
        | [ carPK; doorPK ] -> carPK, doorPK
        | _ -> failwith "invalid input")

    let transformOnce subject number = (subject * number) % 20201227UL

    let transform subject loopSize =
        Seq.fold (fun number _ -> transformOnce subject number) 1UL (seq { 1UL .. loopSize })

    let findLoopSize subject target =
        let rec aux number loopSize =
            let transformed = transformOnce subject number
            if transformed = target then loopSize else aux transformed (loopSize + 1UL)

        aux 1UL 1UL

    let part1 ((carPK, doorPK): uint64 * uint64): uint64 =
        let carLoopSize = findLoopSize 7UL carPK
        transform doorPK carLoopSize

    let part2 (publicKeys: uint64 * uint64): uint64 =
        // No part 2 xD
        0UL

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
