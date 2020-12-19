namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Text.RegularExpressions

module Day19 =
    type RuleId = int

    type Rule =
        | Const of char
        | Combination of seq<seq<RuleId>>

    type Input = seq<RuleId * Rule> * seq<string>

    let parseRule (input: string): Rule =
        if input.Contains('"') then
            Const(input.Chars(input.IndexOf('"') + 1))
        else
            input.Split(" | ")
            |> Seq.map (fun part -> part.Split(" ") |> Seq.map (int))
            |> Combination

    let parseRuleWithId (line: string): RuleId * Rule =
        match line.Split(": ") |> Array.truncate 2 with
        | [| id; rule |] -> (int id, parseRule rule)
        | _ -> failwith "invalid rule"

    let parseFile (filename: string): Input =
        let content = IO.File.ReadAllText filename

        match content.Split("\n\n") |> Array.truncate 2 with
        | [| rulesContent; stringsContent |] ->
            let rules =
                rulesContent.Split("\n")
                |> Seq.map parseRuleWithId

            let strings =
                stringsContent.Split("\n") |> Seq.ofArray

            rules, strings
        | _ -> failwith "invalid input"

    let part1 ((rules, strings): Input): int =
        let rules = Map.ofSeq rules

        let rec ruleToRegex ruleId =
            match Map.find ruleId rules with
            | Const c -> c.ToString()
            | Combination ors ->
                ors
                |> Seq.map (fun seqs ->
                    seqs
                    |> Seq.map ruleToRegex
                    |> (fun x -> String.Join("", x)))
                |> (fun x -> String.Join("|", x))
                |> sprintf "(%s)"

        let ruleRegex = sprintf "^%s$" (ruleToRegex 0)

        strings
        |> Seq.filter (fun string -> Regex.Match(string, ruleRegex).Success)
        |> Seq.length

    let part2 ((rules, strings): Input): int = failwith "Part 2 not implemented"

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
