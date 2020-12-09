namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day9 =
    let parseFile: string -> uint64 array =
        IO.File.ReadLines
        >> (Seq.map uint64)
        >> Array.ofSeq

    let containSum (prev: uint64 array) (number: uint64): bool =
        seq { 0 .. prev.Length - 2 }
        |> Seq.collect (fun i -> seq { for j in i + 1 .. prev.Length - 1 -> (i, j) })
        |> Seq.exists (fun (i, j) -> prev.[i] + prev.[j] = number)

    let part1 (numbers: uint64 array): uint64 =
        seq { for i in 25 .. numbers.Length -> numbers.[i - 25..i - 1], numbers.[i] }
        |> Seq.pick (fun (prev, num) -> if containSum prev num then None else Some num)

    let part2 (numbers: uint64 array): uint64 =
        let expected = part1 numbers

        let rec contiguousSubsetSum i j sum =
            // We need two numbers, so force going further if i = j
            if i = j || sum < expected
            then contiguousSubsetSum i (j + 1) (sum + numbers.[j + 1])
            elif sum > expected
            then contiguousSubsetSum (i + 1) j (sum - numbers.[i])
            else (i, j)

        let sumSmallAndBig slice = (Array.max slice) + (Array.min slice)

        let tupleToSlice (i, j) = numbers.[i..j]

        (numbers.[0] + numbers.[1])
        |> contiguousSubsetSum 0 1
        |> tupleToSlice
        |> sumSmallAndBig


    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
