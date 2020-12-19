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
    Important note: this is not great design because it hides mutability
    behind an "immutable-like" interface (seq), which can yield invalid results

    For example, if I do:
        let results = runGame startingNumbers
        let num1 = results |> Seq.take 2020 |> Seq.last
        let num2 = results |> Seq.take 2020 |> Seq.last

    You would expect that `num1 = num2`, but that is not the case, because you
    would re-evaluate the unfolding function with the same "shared" spoken dict
    which would cause the result to be different.

    I'm just leaving it here for "educational purposes" and will fix in the next commit
    *)

    let runGame (startingNumbers: seq<int>): seq<int> =
        let mutable spoken = Dictionary<int, int>()

        Seq.unfold (fun (lastNum, lastI, startingNumbers) ->
            match startingNumbers with
            | num :: rest ->
                if lastNum <> -1 then spoken.[lastNum] <- lastI
                Some(num, (num, lastI + 1, rest))
            | [] ->
                let nextNum =
                    if spoken.ContainsKey lastNum then lastI - spoken.[lastNum] else 0

                spoken.[lastNum] <- lastI

                Some(nextNum, (nextNum, lastI + 1, []))) (-1, 0, List.ofSeq startingNumbers)

    let part1 (startingNumbers: seq<int>): int =
        startingNumbers
        |> runGame
        |> Seq.take 2020
        |> Seq.last

    let part2 (startingNumbers: seq<int>): int =
        startingNumbers
        |> runGame
        |> Seq.take 30000000
        |> Seq.last

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
