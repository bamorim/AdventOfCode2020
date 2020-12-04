namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day4 =
    open System.Text.RegularExpressions
    type Document = Map<string, string>

    let parseFile filename: seq<Document> =
        let text = System.IO.File.ReadAllText filename

        text.Split("\n\n")
        |> Seq.ofArray
        |> Seq.map (fun part ->
            [ ' '; '\n' ]
            |> Array.ofList
            |> part.Split
            |> Seq.choose (fun field ->
                let keyvalue = field.Split(":")

                if keyvalue.Length = 2 then Some(keyvalue.[0], keyvalue.[1]) else None)
            |> Map.ofSeq)

    let part1 (docs: seq<Document>): int =
        let requiredFields =
            Seq.ofList [ "byr"
                         "iyr"
                         "eyr"
                         "hgt"
                         "hcl"
                         "ecl"
                         "pid" ]

        let validDocument doc =
            Seq.forall (fun key -> Map.containsKey key doc) requiredFields

        docs |> Seq.filter validDocument |> Seq.length

    let part2 (docs: seq<Document>): int =
        let validate (field, validation) doc =
            match Map.tryFind field doc with
            | Some value -> validation value
            | _ -> false

        let validateAll validations doc =
            Seq.forall (fun validation -> validate validation doc) validations

        let validateYear min max str =
            Regex.Match(str, "^\d{4}$").Success
            && (int str) >= min
            && (int str) <= max

        let (|WithUnit|_|) pattern str =
            let m =
                Regex.Match(str, sprintf "^(\d+)%s$" pattern)

            if (m.Success) then Some(int m.Groups.[1].Value) else None

        let validEcls =
            Set.ofList [ "amb"
                         "blu"
                         "brn"
                         "gry"
                         "grn"
                         "hzl"
                         "oth" ]

        let validations =
            [ ("byr", validateYear 1920 2002)
              ("iyr", validateYear 2010 2020)
              ("eyr", validateYear 2020 2030)
              ("hgt",
               function
               | WithUnit "in" hgt -> hgt >= 59 && hgt <= 76
               | WithUnit "cm" hgt -> hgt >= 150 && hgt <= 193
               | _ -> false)
              ("hcl", (fun hcl -> Regex.Match(hcl, "^#[a-f0-9]{6}$").Success))
              ("ecl", (fun ecl -> Set.contains ecl validEcls))
              ("pid", (fun pid -> Regex.Match(pid, "^\d{9}$").Success)) ]

        docs
        |> Seq.filter (validateAll validations)
        |> Seq.length

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
