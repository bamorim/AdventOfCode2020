namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day10 =
    let parseFile: string -> seq<int> = IO.File.ReadLines >> (Seq.map int)

    let part1 (outlets: seq<int>): int =
        let outletDiff = Seq.min outlets
        let deviceDiff = 3

        outlets
        |> Seq.sort
        |> Seq.pairwise
        |> Seq.map (fun (x, next_x) -> next_x - x)
        |> Seq.append (Seq.ofList [ outletDiff; deviceDiff ])
        |> Seq.fold (fun (c1, c3) diff ->
            if diff = 1 then (c1 + 1, c3)
            elif diff = 3 then (c1, c3 + 1)
            else (c1, c3)) (0, 0)
        |> (fun (c1, c3) -> c1 * c3)

    let joltsArray (outlets: seq<int>): int array =
        [ Seq.singleton 0
          Seq.sort outlets
          Seq.singleton (3 + Seq.max outlets) ]
        |> Seq.ofList
        |> Seq.concat
        |> Array.ofSeq

    // Imperative and mutable dynamic programming bottom-up approach
    let part2 (outlets: seq<int>): uint64 =
        let jolts = joltsArray outlets
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
