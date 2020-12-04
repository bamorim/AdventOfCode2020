namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day3 =
    let parseFile = System.IO.File.ReadAllLines

    let isTree x (line: string) = line.Chars(x % line.Length) = '#'

    let genPoints (dx, dy) =
        Seq.initInfinite (fun i -> (i * dx, i * dy))

    let countTrees (lines: string []) (points: seq<int * int>): int =
        points
        |> Seq.takeWhile (fun (_, y) -> y < lines.Length)
        |> Seq.filter (fun (x, y) -> isTree x lines.[y])
        |> Seq.length

    let part1 (lines: string []): int = (3, 1) |> genPoints |> countTrees lines

    let part2 (lines: string []): uint64 =
        let slopes =
            [ (1, 1)
              (3, 1)
              (5, 1)
              (7, 1)
              (1, 2) ]

        slopes
        |> Seq.ofList
        |> Seq.map (genPoints >> (countTrees lines) >> uint64)
        |> Seq.reduce (*)

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
