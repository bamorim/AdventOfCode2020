namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day1 =
    let parseFile (filename: string): seq<int> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map int

    let findSumPair (sum : int) (entries : Set<int>) : (int * int) option =
        entries
        |> Set.toSeq
        |> Seq.tryFind (fun x -> Set.contains (sum - x) entries)
        |> function
            | Some(i) -> Some(i, sum - i)
            | None -> None

    let part1 (entries : seq<int>) : int =
        match findSumPair 2020 (Set.ofSeq entries) with
        | Some(x, y) -> x * y
        | None -> failwith "sum not found"

    let part2 (entries : seq<int>) : int =
        let allEntries = Set.ofSeq entries

        let tryOne (x: int): (int * int * int) option =
            allEntries
            |> Set.remove x
            |> findSumPair (2020 - x)
            |> Option.map (fun (y, z) -> (x, y, z))

        match Seq.tryPick tryOne entries with
        | Some(x, y, z) -> x * y * z
        | None -> failwith "sum not found"

    let day: Day<_, _, _> = { parseFile = parseFile; part1 = part1; part2 = part2 }