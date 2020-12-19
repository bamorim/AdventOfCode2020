namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

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

    let matchRule0 rules (string: string) =
        let rec matchRule (ruleId: RuleId) (chars: char list) =
            match (chars, Map.find ruleId rules) with
            | (_, Combination options) -> matchOptions chars options
            | (c :: chars, Const expected) -> if c = expected then Seq.singleton chars else Seq.empty
            | _ -> Seq.empty

        and matchOptions chars (options: seq<seq<RuleId>>): seq<char list> =
            Seq.collect (matchSequence chars) options

        and matchSequence chars (ruleIds: seq<RuleId>): seq<char list> =
            Seq.fold (fun state ruleId -> Seq.collect (fun chars -> matchRule ruleId chars) state) (Seq.singleton chars)
                ruleIds

        string.ToCharArray()
        |> List.ofArray
        |> matchRule 0
        |> Seq.exists List.isEmpty

    let part1 ((rules, strings): Input): int =
        let rules = Map.ofSeq rules

        strings
        |> Seq.filter (matchRule0 rules)
        |> Seq.length

    let part2 ((rules, strings): Input): int =
        let rules =
            rules
            |> Map.ofSeq
            |> Map.add 8 (parseRule "42 | 42 8")
            |> Map.add 11 (parseRule "42 31 | 42 11 31")

        strings
        |> Seq.filter (matchRule0 rules)
        |> Seq.length

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
