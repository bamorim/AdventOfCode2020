namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Program =
    let runDay (dayNum: int) (day: Day<_, _, _>) =
        let filename =
            System.IO.Path.Combine(__SOURCE_DIRECTORY__, (sprintf "./inputs/%d.txt" dayNum))

        let parsed = day.parseFile filename

        printfn "Day %d Part 1 - %A" dayNum (day.part1 parsed)
        printfn "Day %d Part 2 - %A" dayNum (day.part2 parsed)

    [<EntryPoint>]
    let main argv =
        let dayStr =
            if argv.Length > 0 then argv.[0] else "15"

        match dayStr with
        | "1" -> runDay 1 Day1.day
        | "2" -> runDay 2 Day2.day
        | "3" -> runDay 3 Day3.day
        | "4" -> runDay 4 Day4.day
        | "5" -> runDay 5 Day5.day
        | "6" -> runDay 6 Day6.day
        | "7" -> runDay 7 Day7.day
        | "8" -> runDay 8 Day8.day
        | "9" -> runDay 9 Day9.day
        | "10" -> runDay 10 Day10.day
        | "11" -> runDay 11 Day11.day
        | "12" -> runDay 12 Day12.day
        | "13" -> runDay 13 Day13.day
        | "14" -> runDay 14 Day14.day
        | "15" -> runDay 15 Day15.day
        | _ -> failwith "Invalid day"

        0 // return an integer exit code
