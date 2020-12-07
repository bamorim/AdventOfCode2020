namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Text.RegularExpressions

module Day7 =
    type BagColor = string
    type ContainerBag = BagColor * seq<int * BagColor>

    let parseLine (line: string): ContainerBag =
        let parseContents contentPart =
            let contentMatch =
                Regex.Match(contentPart, "^(\d+) (.*) bags?$")

            if contentMatch.Success
            then (int contentMatch.Groups.[1].Value, contentMatch.Groups.[2].Value)
            else failwith "Invalid line"

        let regexMatch =
            Regex.Match(line, "(.*) bags contain (.*)\.$")

        if regexMatch.Success then
            let bagColor = regexMatch.Groups.[1].Value
            let contentsString = regexMatch.Groups.[2].Value

            let contents =
                if contentsString = "no other bags" then
                    Seq.empty
                else
                    contentsString.Split(", ")
                    |> Seq.ofArray
                    |> Seq.map parseContents

            (bagColor, contents)
        else
            failwith "Invalid line"

    let parseFile (filename: string): seq<ContainerBag> =
        filename |> IO.File.ReadLines |> Seq.map parseLine

    // Builds a graph with reversed edges and without weights
    let getContainedBy (bags: seq<ContainerBag>) =
        bags
        |> Seq.collect (fun (container, contents) -> Seq.map (fun (_, content) -> (container, content)) contents)
        |> Seq.groupBy snd
        |> Seq.map (fun (content, inside) -> (content, Seq.map fst inside))
        |> Map.ofSeq

    // Builds the "canonical" weighted directed graph (hoping that it is acyclic)
    let getContents (bags: seq<ContainerBag>) = Map.ofSeq bags

    let myBag: BagColor = "shiny gold"

    // Run a DFS search
    let part1 (bags: seq<ContainerBag>): int =
        let containedBy = getContainedBy bags

        let rec aux (discovered: Set<BagColor>) (id: BagColor): Set<BagColor> =
            let newDiscovered = Set.add id discovered

            containedBy
            |> Map.tryFind id
            |> Option.defaultValue Seq.empty
            |> Seq.fold (fun discovered next ->
                if Set.contains next discovered then
                    discovered
                else
                    aux discovered next
            ) newDiscovered

        myBag
        |> aux Set.empty
        |> Set.remove myBag
        |> Set.count

    // Traverse throught the graph multiplying the weights and summing all values
    let part2 (bags: seq<ContainerBag>): int =
        let contentMap = getContents bags

        let rec aux (color: BagColor) (count: int): int =
            let contents =
                contentMap
                |> Map.tryFind color
                |> Option.defaultValue Seq.empty

            count
            + Seq.sumBy (fun (amt, color) -> aux color (count * amt)) contents


        (aux myBag 1) - 1

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
