namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Program =
    type Runner = (unit -> unit)
    type Days = Map<int, Runner>

    let buildRunner (dayNum: int) (day: Day<_, _, _>): Runner =
        fun () ->
            let filename =
                System.IO.Path.Combine(__SOURCE_DIRECTORY__, (sprintf "./inputs/%d.txt" dayNum))

            let parsed = day.parseFile filename

            printfn "Day %d Part 1 - %A" dayNum (day.part1 parsed)
            printfn "Day %d Part 2 - %A" dayNum (day.part2 parsed)

    let addDay (dayNum: int) (day: Day<_, _, _>) (days: Days): Days =
        Map.add dayNum (buildRunner dayNum day) days

    let days =
        Map.empty
        |> addDay 1 Day1.day
        |> addDay 2 Day2.day
        |> addDay 3 Day3.day
        |> addDay 4 Day4.day
        |> addDay 5 Day5.day
        |> addDay 6 Day6.day
        |> addDay 7 Day7.day
        |> addDay 8 Day8.day
        |> addDay 9 Day9.day
        |> addDay 10 Day10.day
        |> addDay 11 Day11.day
        |> addDay 12 Day12.day
        |> addDay 13 Day13.day
        |> addDay 14 Day14.day
        |> addDay 15 Day15.day
        |> addDay 16 Day16.day
        |> addDay 17 Day17.day
        |> addDay 18 Day18.day
        |> addDay 19 Day19.day
        |> addDay 20 Day20.day

    let (|DayRunner|_|) (value: string) =
        let success, value = Int32.TryParse value
        if success then Map.tryFind value days else None

    [<EntryPoint>]
    let main argv =
        let argument =
            argv
            |> Array.tryHead
            |> Option.defaultValue "last"

        let allDays = days |> Map.toSeq |> Seq.map fst
        let lastDay = allDays |> Seq.max
        let runDay day = (Map.find day days) ()

        match argument with
        | "all" -> Seq.iter runDay allDays
        | "last" -> runDay lastDay
        | DayRunner runner -> runner ()
        | _ -> failwith "Invalid argument"

        0 // return an integer exit code
