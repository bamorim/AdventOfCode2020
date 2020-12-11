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

    // Imperative and mutable dynamic programming bottom-up approach
    let part2 (adapters: seq<int>): uint64 =
        let jolts = adapters |> joltsFor |> Array.ofSeq
        let combinations = Array.map (fun _ -> 0UL) jolts
        combinations.[0] <- 1UL

        for i in 0 .. combinations.Length - 1 do
            for j in i + 1 .. i + 3 do
                if j < combinations.Length
                   && jolts.[j] - jolts.[i] <= 3 then
                    combinations.[j] <- combinations.[j] + combinations.[i]
        combinations.[combinations.Length - 1]

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
