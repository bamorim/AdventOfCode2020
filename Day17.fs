namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day17 =
    type Pocket2D = Set<int * int>
    type Pocket3D = Set<int * int * int>
    type Pocket4D = Set<int * int * int * int>

    let parseFile (filename: string): Pocket2D =
        filename
        |> IO.File.ReadLines
        |> Seq.indexed
        |> Seq.collect (fun (x, line) ->
            line.ToCharArray()
            |> Seq.indexed
            |> Seq.map (fun (y, char) -> (x, y, char)))
        |> Seq.fold (fun pocket ->
            function
            | (x, y, '.') -> pocket
            | (x, y, '#') -> Set.add (x, y) pocket
            | _ -> failwith "Invalid cube") Set.empty

    let runCycle neighbors pocket =
        let wasActive pos = Set.contains pos pocket

        pocket
        |> Seq.collect neighbors
        |> Set.ofSeq
        |> Seq.filter (fun pos ->
            let activeNeighborCount =
                pos
                |> neighbors
                |> Seq.filter wasActive
                |> Seq.length

            match (wasActive pos, activeNeighborCount) with
            | (true, 2) -> true
            | (true, 3) -> true
            | (true, _) -> false
            | (false, 3) -> true
            | (false, _) -> false)
        |> Set.ofSeq

    let neighbors3d (x, y, z) =
        seq {
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    for dz in -1 .. 1 do
                        if not (dx = 0 && dy = 0 && dz = 0) then yield (x + dx, y + dy, z + dz)
        }

    let toPocket3d pocket = Set.map (fun (x, y) -> (x, y, 0)) pocket

    let part1 (pocket: Pocket2D): int =
        seq { 1 .. 6 }
        |> Seq.fold (fun pocket _ -> runCycle neighbors3d pocket) (toPocket3d pocket)
        |> Set.count

    let neighbors4d (x, y, z, w) =
        seq {
            for dx in -1 .. 1 do
                for dy in -1 .. 1 do
                    for dz in -1 .. 1 do
                        for dw in -1 .. 1 do
                            if not (dx = 0 && dy = 0 && dz = 0 && dw = 0)
                            then yield (x + dx, y + dy, z + dz, w + dw)
        }

    let toPocket4d pocket =
        Set.map (fun (x, y) -> (x, y, 0, 0)) pocket

    let part2 (pocket: Pocket2D): int =
        seq { 1 .. 6 }
        |> Seq.fold (fun pocket _ -> runCycle neighbors4d pocket) (toPocket4d pocket)
        |> Set.count

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
