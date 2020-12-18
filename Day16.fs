namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day16 =
    type RuleRange = int * int
    type Ticket = int array
    type Rule = seq<RuleRange>
    type Rules = Map<string, Rule>
    type Input = Rules * Ticket * seq<Ticket>

    let parseRules (part: string): Rules =
        part.Trim().Split("\n")
        |> Seq.ofArray
        |> Seq.map (fun line ->
            match line.Split(": ") with
            | [| field; ranges |] ->
                (field,
                 ranges.Split(" or ")
                 |> Seq.ofArray
                 |> Seq.map (fun range ->
                     match range.Split("-") with
                     | [| lower; upper |] -> (int lower, int upper)
                     | _ -> failwith "invalid range"))
            | _ -> failwith "invalid rule format")
        |> Map.ofSeq

    let parseTicket (line: string): Ticket = Array.map int <| line.Split(",")

    let parseMyTicket (part: string): Ticket =
        parseTicket <| part.Trim().Split("\n").[1]

    let parseNearbyTickets (part: string): seq<Ticket> =
        part.Trim().Split("\n")
        |> Seq.ofArray
        |> Seq.skip 1
        |> Seq.map parseTicket

    let parseFile (filename: string): Input =
        match Array.truncate 3
              <| (IO.File.ReadAllText filename).Split("\n\n") with
        | [| rulePart; myTicketPart; nearbyTicketsPart |] ->
            parseRules rulePart, parseMyTicket myTicketPart, parseNearbyTickets nearbyTicketsPart
        | _ -> failwith "invalid input"

    let matchRule (rule: Rule) number =
        Seq.exists (fun (min, max) -> number >= min && number <= max) rule

    let isValidNumber (rules: Rules) number =
        rules
        |> Map.toSeq
        |> Seq.exists (fun (_, rule) -> matchRule rule number)

    let isValidTicket (rules: Rules) (ticket: Ticket) =
        Array.forall (isValidNumber rules) ticket

    let getRuleMapping (rules: Rules) (tickets: seq<Ticket>): Map<int, string> =
        let ruleNames = rules |> Map.toSeq |> Seq.map fst

        let rec aux (result: Map<int, string>) (possibilities: Map<int, Set<string>>): Map<int, string> =
            possibilities
            |> Map.toSeq
            |> Seq.tryFind (fun (_i, names) -> (Set.count names) = 1)
            |> (function
            | Some (i, names) ->
                let name = Seq.head names

                let newPossibilities =
                    possibilities
                    |> Map.remove i
                    |> Map.map (fun _i -> Set.remove name)

                aux (Map.add i name result) newPossibilities
            | None -> result)

        tickets
        |> Seq.filter (isValidTicket rules)
        |> Seq.collect Array.indexed
        |> Seq.fold (fun (possibilities: Map<int, seq<string>>) (i: int, number: int) ->
            Map.add
                i
                (possibilities
                 |> Map.tryFind i
                 |> Option.defaultValue (ruleNames)
                 |> Seq.filter (fun name -> matchRule (Map.find name rules) number))
                possibilities) (Map.empty)
        |> Map.map (fun _key -> Set.ofSeq)
        |> aux Map.empty

    let part1 ((rules, _myTicket, nearbyTickets): Input): int =
        nearbyTickets
        |> Seq.collect Seq.ofArray
        |> Seq.filter (not << isValidNumber rules)
        |> Seq.sum

    let part2 ((rules, myTicket, nearbyTickets): Input): uint64 =
        let mapping = getRuleMapping rules nearbyTickets

        myTicket
        |> Seq.ofArray
        |> Seq.indexed
        |> Seq.choose (fun (i, num) ->
            let ruleName = Map.find i mapping

            if ruleName.StartsWith("departure") then Some num else None)
        |> Seq.map uint64
        |> Seq.reduce (*)

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
