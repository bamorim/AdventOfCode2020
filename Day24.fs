namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day24 =
    type Direction =
        | NorthWest
        | NorthEast
        | East
        | SouthEast
        | SouthWest
        | West

    type Path = seq<Direction>
    // We'll be using an axial coordinate system
    type Pos = int * int

    let parsePath (str: string): Path =
        let rec aux directions chars =
            match chars with
            | [] -> List.rev directions
            | 's' :: 'e' :: rest -> aux (SouthEast :: directions) rest
            | 's' :: 'w' :: rest -> aux (SouthWest :: directions) rest
            | 'n' :: 'e' :: rest -> aux (NorthEast :: directions) rest
            | 'n' :: 'w' :: rest -> aux (NorthWest :: directions) rest
            | 'w' :: rest -> aux (West :: directions) rest
            | 'e' :: rest -> aux (East :: directions) rest
            | _ -> failwith "invalid path"

        str.Trim().ToCharArray()
        |> List.ofArray
        |> aux []
        |> Seq.ofList

    let parseFile: string -> seq<Path> = IO.File.ReadLines >> Seq.map parsePath

    let delta: Direction -> Pos =
        function
        | NorthWest -> (0, 1)
        | NorthEast -> (1, 1)
        | East -> (1, 0)
        | SouthEast -> (0, -1)
        | SouthWest -> (-1, -1)
        | West -> (-1, 0)

    let add ((x1, y1): Pos) ((x2, y2): Pos): Pos = (x1 + x2, y1 + y2)

    let move (pos: Pos): Direction -> Pos = delta >> (add pos)

    let pathToPos (path: Path): Pos = Seq.fold move (0, 0) path

    let part1 (paths: seq<Path>): int =
        paths
        |> Seq.countBy pathToPos
        |> Seq.filter (fun (_pos, c) -> (c % 2) = 1)
        |> Seq.length

    let neighbors (pos: Pos): seq<Pos> =
        Seq.map
            (move pos)
            [ NorthWest
              NorthEast
              East
              SouthEast
              SouthWest
              West ]

    let mutate (blackTiles: Set<Pos>): Set<Pos> =
        blackTiles
        |> Seq.collect neighbors
        |> Seq.append blackTiles
        |> Seq.distinct
        |> Seq.fold (fun newBlackTiles tile ->
            let isBlack tile = Set.contains tile blackTiles
            let isWhite = not << isBlack

            let blackNeighbors =
                tile
                |> neighbors
                |> Seq.filter isBlack
                |> Seq.length

            if (isBlack tile)
               && (blackNeighbors = 0 || blackNeighbors > 2) then
                Set.remove tile newBlackTiles
            elif (isWhite tile) && (blackNeighbors = 2) then
                Set.add tile newBlackTiles
            else
                newBlackTiles) blackTiles

    let part2 (paths: seq<Path>): int =
        let initialBlackTiles =
            paths
            |> Seq.countBy pathToPos
            |> Seq.filter (fun (_pos, c) -> (c % 2) = 1)
            |> Seq.map fst
            |> Set.ofSeq

        seq { 1 .. 100 }
        |> Seq.fold (fun blackTiles _ -> mutate blackTiles) initialBlackTiles
        |> Set.count

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
