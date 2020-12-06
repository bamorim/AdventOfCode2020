namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day5 =
    type SeatId = int

    let parseLine (line: string): SeatId =
        Convert.ToInt32
            (line
                .Replace('B', '1')
                 .Replace('R', '1')
                 .Replace('F', '0')
                 .Replace('L', '0'),
             2)

    let parseFile (filename: string): seq<SeatId> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map parseLine

    let part1 (tickets: seq<SeatId>): int = Seq.max tickets

    let part2 (tickets: seq<SeatId>): int =
        let sortedTickets = Seq.sort tickets

        sortedTickets
        |> Seq.pairwise
        |> Seq.find (fun (x, y) -> (x + 2) = y)
        |> fst
        |> (+) 1

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
