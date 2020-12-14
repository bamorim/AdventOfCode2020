namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day13 =

    let parseFile (filename: string): int * seq<int option> =
        let lines = IO.File.ReadLines filename
        let earliest = lines |> Seq.head |> int

        let busIds =
            (lines |> Seq.skip 1 |> Seq.head).Split(',')
            |> Seq.ofArray
            |> Seq.map (fun idStr ->
                let success, id = System.Int32.TryParse idStr
                if success then Some id else None)

        (earliest, busIds)

    let part1 ((earliest, busIds): int * seq<int option>): int =
        let minutesUntil id = id - (earliest % id)

        let id =
            busIds |> Seq.choose id |> Seq.minBy minutesUntil

        id * (minutesUntil id)

    let rec gcd (x, y) =
        if x = y then x
        else if x > y then gcd (x - y, y)
        else gcd (x, y - x)

    let possibleTs (int, offset) =
        Seq.initInfinite (fun i -> int * (int64 i) - offset)

    // Just brute force it
    // A better approach would be to use the Extended Euclidean Algorithm
    // To find the inverse of n in mod m
    // But since our ids are really small, brute forcing our way into it is not that bad
    let inverseMod (n: int64) (m: int64) =
        int64
        |> Seq.initInfinite
        |> Seq.find (fun x -> x * n % m = 1L)

    // Okay, this was a tricky one.
    // I found out that all busIds were prime, therefore they are all pairwise coprime
    // With that in mind, we can use the Chinese Remainder Theorem to solve for t
    //
    // I don't even understand this 100% (not even 50%), but it seems to be working xD
    let part2 ((_earliest, busIds): int * seq<int option>): int64 =
        let idsWithReminders =
            busIds
            |> Seq.mapi (fun offset ->
                function
                | Some id -> Some(int64 id, int64 (id - (offset % id)))
                | _ -> None)
            |> Seq.choose id

        let m =
            idsWithReminders |> Seq.map fst |> Seq.reduce (*)

        idsWithReminders
        |> Seq.map (fun (id, rem) ->
            let mi = m / id
            let imi = inverseMod mi id
            mi * imi * rem)
        |> Seq.reduce (+)
        |> (fun x -> x % m)

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
