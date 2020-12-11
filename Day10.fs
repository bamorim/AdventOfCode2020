namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day10 =
    let parseFile: string -> seq<int> = IO.File.ReadLines >> (Seq.map int)

    let joltsFor (adapters: seq<int>) =
        (Seq.concat << Seq.ofList)
            [ Seq.singleton 0
              Seq.sort adapters
              adapters |> Seq.max |> ((+) 3) |> Seq.singleton ]

    let part1 (adapters: seq<int>): int =
        adapters
        |> joltsFor
        |> Seq.pairwise
        |> Seq.map (fun (x, next_x) -> next_x - x)
        |> Seq.fold (fun (c1, c3) diff ->
            if diff = 1 then (c1 + 1, c3)
            elif diff = 3 then (c1, c3 + 1)
            else (c1, c3)) (0, 0)
        |> (fun (c1, c3) -> c1 * c3)

    let part2 (adapters: seq<int>): uint64 =
        adapters
        |> joltsFor
        |> Seq.fold (fun last3 jolts ->
            match last3 with
            | [] -> [ (1UL, jolts) ]
            | _ ->
                let combinations =
                    last3
                    |> List.filter (fun (_, otherJolts) -> jolts - otherJolts <= 3)
                    |> List.sumBy fst

                (combinations, jolts) :: (List.truncate 2 last3)) []
        |> List.head
        |> fst

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
