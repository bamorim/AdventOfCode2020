namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day12 =
    type Direction =
        | North
        | South
        | East
        | West

    type Instruction =
        | To of Direction * int
        | Forward of int
        | Left of int
        | Right of int

    let parseLine (line: string): Instruction =
        let num = int <| line.Substring(1)

        match line.Chars(0) with
        | 'N' -> To(North, num)
        | 'S' -> To(South, num)
        | 'E' -> To(East, num)
        | 'W' -> To(West, num)
        | 'F' -> Forward num
        | 'L' -> Left(num / 90)
        | 'R' -> Right(num / 90)
        | _ -> failwith "invalid instruction"

    let parseFile: string -> seq<Instruction> = IO.File.ReadLines >> (Seq.map parseLine)

    let move direction d (x, y) =
        match direction with
        | North -> (x, y + d)
        | South -> (x, y - d)
        | East -> (x + d, y)
        | West -> (x - d, y)

    let part1 (instructions: seq<Instruction>): int =
        let leftOf =
            function
            | North -> West
            | West -> South
            | South -> East
            | East -> North

        let rightOf =
            function
            | North -> East
            | East -> South
            | South -> West
            | West -> North

        let applyInstruction (facing, pos) =
            function
            | To (direction, num) -> (facing, move direction num pos)
            | Forward num -> (facing, move facing num pos)
            | Left num -> (Seq.fold (fun f _ -> leftOf f) facing { 1 .. num }, pos)
            | Right num -> (Seq.fold (fun f _ -> rightOf f) facing { 1 .. num }, pos)

        match Seq.fold applyInstruction (East, (0, 0)) instructions with
        | (_, (x, y)) -> (abs x) + (abs y)

    let part2 (instructions: seq<Instruction>): int =
        let moveShip num (x, y) (dx, dy) = (x + dx * num, y + dy * num)
        let rotateWaypointLeft (dx, dy) = (-dy, dx)
        let rotateWaypointRight (dx, dy) = (dy, -dx)

        let applyInstruction (waypoint, pos) =
            function
            | To (direction, num) -> (move direction num waypoint, pos)
            | Forward num -> (waypoint, moveShip num pos waypoint)
            | Left num -> (Seq.fold (fun f _ -> rotateWaypointLeft f) waypoint { 1 .. num }, pos)
            | Right num -> (Seq.fold (fun f _ -> rotateWaypointRight f) waypoint { 1 .. num }, pos)

        match Seq.fold applyInstruction ((10, 1), (0, 0)) instructions with
        | (_, (x, y)) -> (abs x) + (abs y)

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
