namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day15 =
    let parseFile (filename: string): seq<int> =
        filename
        |> IO.File.ReadAllText
        |> (fun x -> x.Split ',')
        |> Seq.ofArray
        |> Seq.map int

    let runGame (startingNumbers: seq<int>): seq<int> =
        Seq.unfold (fun (lastNum, lastI, startingNumbers, spoken) ->
            let nextSpoken = Map.add lastNum lastI spoken

            match startingNumbers with
            | num :: rest -> Some(num, (num, lastI + 1, rest, (if lastNum = -1 then spoken else nextSpoken)))
            | [] ->
                let nextNum =
                    match Map.tryFind lastNum spoken with
                    | Some i -> lastI - i
                    | None -> 0

                Some(nextNum, (nextNum, lastI + 1, [], nextSpoken))) (-1, 0, List.ofSeq startingNumbers, Map.empty)

    let part1 (startingNumbers: seq<int>): int =
        startingNumbers
        |> runGame
        |> Seq.take 2020
        |> Seq.last

    let part2 (startingNumbers: seq<int>): int =
        // YOLO - Make it faster later
        startingNumbers
        |> runGame
        |> Seq.take 30000000
        |> Seq.last

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
