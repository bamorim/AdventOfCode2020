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

    (*
    Now this is better than last interation because it keeps the mutability
    inside the runGame function, so it is impossible to create a bug by
    just running the game twice because the Seq unfolding is kept private
    *)

    let runGame (startingNumbers: seq<int>) (n: int): int =
        let mutable spoken = Dictionary<int, int>()

        (-1, 0, List.ofSeq startingNumbers)
        |> Seq.unfold (fun (lastNum, lastI, startingNumbers) ->
            match startingNumbers with
            | num :: rest ->
                if lastNum <> -1 then spoken.[lastNum] <- lastI
                Some(num, (num, lastI + 1, rest))
            | [] ->
                let nextNum =
                    if spoken.ContainsKey lastNum then lastI - spoken.[lastNum] else 0
                spoken.[lastNum] <- lastI

                Some(nextNum, (nextNum, lastI + 1, [])))
        |> Seq.take n
        |> Seq.last

    let part1 (startingNumbers: seq<int>): int = runGame startingNumbers 2020

    let part2 (startingNumbers: seq<int>): int = runGame startingNumbers 30000000

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
