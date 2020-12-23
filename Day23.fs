namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day23 =
    type Cup = int
    type State = Cup list

    let parseFile (filename: string): State =
        (IO.File.ReadAllText filename)
            .Trim()
            .ToCharArray()
        |> Seq.map (fun x -> (int x) - 48)
        |> List.ofSeq

    let part1 (initialState: State): int =
        let invalid _ = failwith "invalid state"

        let cycle =
            function
            | head :: tail -> List.append tail [ head ]
            | _ -> invalid ()

        let pick =
            function
            | current :: one :: two :: three :: rest -> current :: rest, [ one; two; three ]
            | _ -> invalid ()


        let chooseDestination =
            function
            | current :: rest ->
                let rec choose num =
                    if List.contains num rest then num
                    elif num > 1 then choose (num - 1)
                    else choose 9

                choose current
            | _ -> invalid ()

        let placeAt destination picked cups =
            let destinationIndex = List.findIndex ((=) destination) cups
            let (before, after) = List.splitAt (destinationIndex + 1) cups
            List.concat [ before; picked; after ]

        let doMove (cups: State): State =
            let (cups, picked) = pick cups
            let destination = chooseDestination cups

            cups |> placeAt destination picked |> cycle

        let rec doMoves (n: int) (cups: State): State =
            if n <= 0 then cups else cups |> doMove |> doMoves (n - 1)

        let digitsToInt: seq<int> -> int =
            Seq.rev
            >> Seq.mapi (fun i x -> x * (int (10.0 ** (float i))))
            >> Seq.sum

        let rec afterOne =
            function
            | 1 :: rest -> rest
            | cups -> cups |> cycle |> afterOne

        initialState
        |> doMoves 100
        |> afterOne
        |> digitsToInt

    // The immutable version had exponential complexity due to rewriting the list
    // everytime due to the cyclic nature of the problem.
    // This is basically just a rewrite of the same thing using mutable constructs
    // I'm leaving them both because I find funny how they differ but I could techinically
    // just use this one.
    let part2 (initialState: State): uint64 =
        let max = 1000000

        let initialState =
            Seq.append initialState (seq { 10 .. max })

        let rec pickDestination num first second third =
            if num = 0 then pickDestination max first second third
            elif num <> first && num <> second && num <> third then num
            elif num > 1 then pickDestination (num - 1) first second third
            else pickDestination max first second third

        let mutable current = Seq.head initialState
        let next = Array.create (max + 1) 0

        for a, b in Seq.pairwise initialState do
            next.[a] <- b
        next.[Seq.last initialState] <- current

        for _ in 1 .. 10000000 do
            let first = next.[current]
            let second = next.[first]
            let third = next.[second]
            next.[current] <- next.[third]

            let destination =
                pickDestination (current - 1) first second third
            next.[third] <- next.[destination]
            next.[destination] <- first
            current <- next.[current]

        (uint64 next.[1]) * (uint64 next.[next.[1]])

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
