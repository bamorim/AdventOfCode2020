namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day2 =
    open System.Text.RegularExpressions

    type Entry = int * int * char * string

    let lineRegex = "(\d+)-(\d+) (\w): (\w+)"

    let parseLine (line: string): Entry =
        let matched = Regex.Match(line, lineRegex)
        let groups = matched.Groups
        ((int groups.[1].Value), (int groups.[2].Value), groups.[3].Value.Chars(0), groups.[4].Value)

    let parseFile (filename: string): seq<Entry> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map parseLine

    let part1 (entries: seq<Entry>): int =
        let validEntry ((min, max, chr, str): Entry): Boolean =
            let occurrences =
                str.ToCharArray()
                |> Seq.ofArray
                |> Seq.filter (fun x -> x = chr)
                |> Seq.length

            occurrences >= min && occurrences <= max

        entries |> Seq.filter validEntry |> Seq.length

    let part2 (entries: seq<Entry>): int =
        let charCheck (str: String) chr idx = str.Chars(idx - 1) = chr

        let validEntry ((idx1, idx2, chr, str): Entry): Boolean =
            (charCheck str chr idx1)
            <> (charCheck str chr idx2)

        entries |> Seq.filter validEntry |> Seq.length

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
