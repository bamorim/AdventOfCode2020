namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day5 =
    type SeatId = int

    let parseLine (line: string): SeatId =
        line.ToCharArray()
        |> Seq.mapi (fun i c -> if c = 'B' || c = 'R' then 1 <<< (9 - i) else 0)
        |> Seq.sum

    let parseFile (filename: string): seq<SeatId> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map parseLine

    let part1: seq<SeatId> -> SeatId = Seq.max

    let part2: seq<SeatId> -> SeatId =
        Seq.sort
        >> Seq.pairwise
        >> Seq.pick (fun (x, y) -> if y = x + 2 then Some(x + 1) else None)

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
