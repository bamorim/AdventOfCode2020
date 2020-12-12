namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day11 =
    type Tile =
        | Floor
        | Seat of Boolean

    let parseTile (char: char): Tile =
        match char with
        | '.' -> Floor
        | 'L' -> Seat false
        | '#' -> Seat true
        | _ -> failwithf "Invalid tile %c" char

    let parseLine (line: string): seq<Tile> =
        line.ToCharArray()
        |> Seq.ofArray
        |> Seq.map parseTile

    let parseFile: string -> seq<seq<Tile>> = IO.File.ReadLines >> (Seq.map parseLine)

    let buildState (grid: seq<seq<Tile>>): Tile [,] =
        let state =
            Array2D.init (grid |> Seq.head |> Seq.length) (grid |> Seq.length) (fun _ _ -> Floor)

        for (y, row) in grid |> Seq.map Seq.indexed |> Seq.indexed do
            for (x, tile) in row do
                state.[x, y] <- tile

        state

    let directions: seq<int * int> =
        seq {
            for i in -1 .. 1 do
                yield!
                    seq {
                        for j in -1 .. 1 do
                            if (i, j) <> (0, 0) then yield (i, j)
                    }
        }

    let isOccupied =
        function
        | Floor -> false
        | Seat occupied -> occupied

    let rec runSimulation (getNeighbors: (int -> int -> Tile [,] -> seq<Tile>))
                          (shouldLeave: int -> Boolean)
                          (state: Tile [,])
                          =
        let mutable changed = false

        let nextState =
            Array2D.mapi (fun x y tile ->
                let occupied =
                    state
                    |> getNeighbors x y
                    |> Seq.filter isOccupied
                    |> Seq.length

                match (tile, shouldLeave occupied, occupied > 0) with
                | (Seat true, true, _) ->
                    changed <- true
                    Seat false
                | (Seat false, _, false) ->
                    changed <- true
                    Seat true
                | _ -> tile) state

        if changed
        then runSimulation getNeighbors shouldLeave nextState
        else nextState

    let occupiedInTheEnd getNeighbors shouldLeave grid =
        grid
        |> buildState
        |> runSimulation getNeighbors shouldLeave
        |> Seq.cast<Tile>
        |> Seq.filter isOccupied
        |> Seq.length

    let part1 (grid: seq<seq<Tile>>): int =
        let getNeighbors x y (state: 'a [,]) =
            let maxX = Array2D.length1 state
            let maxY = Array2D.length2 state

            seq {
                for i, j in directions do
                    if (x + i) >= 0
                       && (x + i) < maxX
                       && (y + j) >= 0
                       && (y + j) < maxY then
                        yield state.[x + i, y + j]
            }

        let shouldLeave occupied = occupied >= 4

        occupiedInTheEnd getNeighbors shouldLeave grid

    let part2 (grid: seq<seq<Tile>>): int =

        let getNeighbors x y (state: Tile [,]) =
            let maxX = Array2D.length1 state
            let maxY = Array2D.length2 state

            directions
            |> Seq.choose (fun (dx, dy) ->
                (fun i -> (x + dx * (i + 1), y + dy * (i + 1)))
                |> Seq.initInfinite
                |> Seq.takeWhile (fun (x, y) -> x >= 0 && x < maxX && y >= 0 && y < maxY)
                |> Seq.tryPick (fun (x, y) ->
                    match state.[x, y] with
                    | Seat _ -> Some state.[x, y]
                    | _ -> None))

        let shouldLeave occupied = occupied >= 5

        occupiedInTheEnd getNeighbors shouldLeave grid

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
