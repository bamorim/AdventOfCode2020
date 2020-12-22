namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day22 =
    type Deck = int list
    type State = Deck * Deck

    let parseFile: string -> State =
        IO.File.ReadAllText
        >> (fun content ->
            let parseDeck (content: string) =
                content.Trim().Split("\n")
                |> Seq.skip 1
                |> Seq.map int
                |> List.ofSeq

            match Array.truncate 2 (content.Split("\n\n")) with
            | [| player1; player2 |] -> (parseDeck player1, parseDeck player2)
            | _ -> failwith "invalid input")

    let score: Deck -> int =
        Seq.rev
        >> Seq.mapi (fun i x -> (i + 1) * x)
        >> Seq.sum

    let part1 (initialState: State): int =
        let runRound (state: State): State =
            match state with
            | (drawn1 :: deck1, drawn2 :: deck2) ->
                if drawn1 > drawn2
                then (List.append deck1 [ drawn1; drawn2 ], deck2)
                else (deck1, List.append deck2 [ drawn2; drawn1 ])
            | _ -> failwith "Game is over"

        let rec runGame (state: State): State =
            match state with
            | (_ :: _, _ :: _) -> state |> runRound |> runGame
            | _ -> state

        let winner (state: State): Deck =
            match state with
            | ([], winner) -> winner
            | (winner, []) -> winner
            | _ -> failwith "Game is not over yet"

        initialState |> runGame |> winner |> score

    type RecursiveDeck = int * Deck
    type RecursiveState = RecursiveDeck * RecursiveDeck

    type Winner =
        | Player1 of RecursiveDeck
        | Player2 of RecursiveDeck

    let part2 (initialState: State): int =
        let rec runRound (previousStates: Set<RecursiveState>) (state: RecursiveState): RecursiveState =
            if Set.contains state previousStates then
                // Remove all cards from Player2 to make Player1 win
                (fst state, (0, []))
            else
                match state with
                | (n1, drawn1 :: deck1), (n2, drawn2 :: deck2) ->
                    let player1IfWon: RecursiveDeck =
                        (n1 + 1, List.append deck1 [ drawn1; drawn2 ])

                    let player2IfWon: RecursiveDeck =
                        (n2 + 1, List.append deck2 [ drawn2; drawn1 ])

                    let ifOneWins: RecursiveState = (player1IfWon, (n2 - 1, deck2))
                    let ifTwoWins: RecursiveState = ((n1 - 1, deck1), player2IfWon)

                    if n1 - 1 >= drawn1 && n2 - 1 >= drawn2 then
                        let subPlayer1: RecursiveDeck = (drawn1, List.take drawn1 deck1)
                        let subPlayer2: RecursiveDeck = (drawn2, List.take drawn2 deck2)
                        let recursiveState: RecursiveState = (subPlayer1, subPlayer2)

                        match runGame Set.empty recursiveState with
                        | Player1 _ -> ifOneWins
                        | Player2 _ -> ifTwoWins
                    elif drawn1 > drawn2 then
                        ifOneWins
                    else
                        ifTwoWins
                | _ -> failwith "Game is over"

        and runGame (previousStates: Set<RecursiveState>) (state: RecursiveState): Winner =
            match runRound previousStates state with
            | winner, (0, _) -> Player1 winner
            | (0, _), winner -> Player2 winner
            | newState -> runGame (Set.add state previousStates) newState

        let winner (winner: Winner): Deck =
            match winner with
            | Player1 (_, deck) -> deck
            | Player2 (_, deck) -> deck

        let toRecursiveState ((player1, player2): State): RecursiveState =
            ((List.length player1, player1), (List.length player2, player2))

        initialState
        |> toRecursiveState
        |> runGame Set.empty
        |> winner
        |> score

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
