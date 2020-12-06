namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day6 =
    type Answers = Set<char>
    type Group = seq<Answers>

    let parseFile (filename: string): seq<Group> =
        filename
        |> IO.File.ReadLines
        |> Seq.fold (fun (current, result) line ->
            if String.IsNullOrWhiteSpace line then
                ([], (List.rev current) :: result)
            else
                let chars = Set.ofArray <| line.ToCharArray()
                (chars :: current, result)) ([], [])
        |> (fun (last, result) -> last :: result)
        |> List.map Seq.ofList
        |> Seq.ofList

    let part1 (groups: seq<Group>): int =
        Seq.sumBy ((Seq.reduce Set.union) >> Set.count) groups

    let part2 (groups: seq<Group>): int =
        Seq.sumBy ((Seq.reduce Set.intersect) >> Set.count) groups

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
