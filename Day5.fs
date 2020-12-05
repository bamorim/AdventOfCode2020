namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day5 =
    type SeatId = int

    let parseLine (line: string): SeatId =
        let chars = Seq.ofArray <| line.ToCharArray()

        let isUpper (c: char): bool = c = 'B' || c = 'R'

        let folder (min, max) =
            function
            | 'B'
            | 'R' -> (min + (max - min) / 2 + 1, max)
            | 'F'
            | 'L' -> (min, max - (max - min) / 2 - 1)
            | _ -> failwith "Invalid character"

        let (row, _) =
            chars |> Seq.take 7 |> Seq.fold folder (0, 127)

        let (col, _) =
            chars |> Seq.skip 7 |> Seq.fold folder (0, 7)

        row * 8 + col

    let parseFile (filename: string): seq<SeatId> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map parseLine

    let part1 (tickets: seq<SeatId>): int = Seq.max tickets

    let part2 (tickets: seq<SeatId>): int =
        let sortedTickets = Seq.sort tickets

        sortedTickets
        |> Seq.skip 1
        |> Seq.zip sortedTickets
        |> Seq.find (fun (x, y) -> (x + 2) = y)
        |> fst
        |> (+) 1

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
