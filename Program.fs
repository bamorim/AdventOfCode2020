namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Program =
    let runDay (dayNum : int) (day: Day<_, _, _>) =
      let filename = System.IO.Path.Combine(__SOURCE_DIRECTORY__, (sprintf "./inputs/%d.txt" dayNum))
      let parsed = day.parseFile filename

      printfn "Day %d Part 1 - %A" dayNum (day.part1 parsed)
      printfn "Day %d Part 2 - %A" dayNum (day.part2 parsed)

    [<EntryPoint>]
    let main argv =
        let dayStr = if argv.Length > 0 then argv.[0] else "1"
        
        match dayStr with
        | "1" -> runDay 1 Day1.day
        | "2" -> runDay 2 Day2.day
        | "3" -> runDay 3 Day3.day
        | _ -> failwith "Invalid day"
        0 // return an integer exit code