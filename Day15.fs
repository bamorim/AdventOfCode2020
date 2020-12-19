namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Collections.Generic

module Day15 =
    let parseFile (filename: string): seq<int> =
        filename
        |> IO.File.ReadAllText
        |> (fun x -> x.Split ',')
        |> Seq.ofArray
        |> Seq.map int

    let runGame (n: int) (startingNumbers: seq<int>): int =
        let mutable spoken = Dictionary<int, int>()
        let mutable startingNumbers = List.ofSeq startingNumbers
        let mutable num = -1

        for i in 1 .. n do
            match startingNumbers with
            | nextNum :: rest ->
                if num <> -1 then spoken.[num] <- i
                startingNumbers <- rest
                num <- nextNum
            | [] ->
                let nextNum =
                    if spoken.ContainsKey num then i - spoken.[num] else 0

                spoken.[num] <- i
                num <- nextNum

        num

    let part1: seq<int> -> int = runGame 2020
    let part2: seq<int> -> int = runGame 30000000

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
