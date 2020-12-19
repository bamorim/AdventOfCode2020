namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Collections.Generic

module Day15 =
    let parseFile (filename: string): int [] =
        filename
        |> IO.File.ReadAllText
        |> (fun x -> x.Split ',')
        |> Array.map int

    let runGame (n: int) (startingNumbers: int []): int =
        let mutable spoken = Dictionary<int, int>()
        let mutable num = -1

        for i in 0 .. (n - 1) do
            let nextNum =
                if i < startingNumbers.Length then startingNumbers.[i]
                elif spoken.ContainsKey num then i - spoken.[num]
                else 0

            if num <> -1 then spoken.[num] <- i
            num <- nextNum

        num

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = runGame 2020
          part2 = runGame 30000000 }
