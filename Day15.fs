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
        let spoken = Dictionary<int, int>()

        for (i, num) in Array.indexed startingNumbers.[..^0] do spoken.[num] <- i
        
        let mutable lastNum =  startingNumbers.[^0]

        for i in startingNumbers.Length .. (n - 1) do
            let num =
                if spoken.ContainsKey lastNum then i - 1 - spoken.[lastNum]
                else 0

            spoken.[lastNum] <- i - 1
            lastNum <- num

        lastNum

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = runGame 2020
          part2 = runGame 30000000 }
