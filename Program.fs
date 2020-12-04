open System

type Day<'a, 'b, 'c> = { parseFile: string -> 'a; part1: 'a -> 'b; part2: 'a -> 'c}

module Day1 =
    let parseFile (filename: string): seq<int> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map int

    let findSumPair (sum : int) (entries : Set<int>) : (int * int) option =
        entries
        |> Set.toSeq
        |> Seq.tryFind (fun x -> Set.contains (sum - x) entries)
        |> function
            | Some(i) -> Some(i, sum - i)
            | None -> None

    let part1 (entries : seq<int>) : int =
        match findSumPair 2020 (Set.ofSeq entries) with
        | Some(x, y) -> x * y
        | None -> failwith "sum not found"

    let part2 (entries : seq<int>) : int =
        let allEntries = Set.ofSeq entries

        let tryOne (x: int): (int * int * int) option =
            allEntries
            |> Set.remove x
            |> findSumPair (2020 - x)
            |> Option.map (fun (y, z) -> (x, y, z))

        match Seq.tryPick tryOne entries with
        | Some(x, y, z) -> x * y * z
        | None -> failwith "sum not found"

    let day = { parseFile = parseFile; part1 = part1; part2 = part2 }

module Day2 =
    open System.Text.RegularExpressions

    type Entry = int * int * char * string

    let lineRegex = "(\d+)-(\d+) (\w): (\w+)"

    let parseLine (line: string): Entry =
        let matched = Regex.Match(line, lineRegex)
        let groups = matched.Groups
        ((int groups.[1].Value), (int groups.[2].Value), groups.[3].Value.Chars(0), groups.[4].Value)

    let parseFile (filename: string): seq<Entry> =
        filename
        |> System.IO.File.ReadAllLines
        |> Seq.map parseLine

    let part1 (entries : seq<Entry>) : int =
        let validEntry ((min, max, chr, str): Entry): Boolean =
            let occurrences =
                str.ToCharArray()
                |> Seq.ofArray
                |> Seq.filter (fun x -> x = chr)
                |> Seq.length
            occurrences >= min && occurrences <= max
        entries
        |> Seq.filter validEntry
        |> Seq.length

    let part2 (entries : seq<Entry>) : int =
        let charCheck (str: String) chr idx =
            str.Chars(idx - 1) = chr
        let validEntry ((idx1, idx2, chr, str): Entry): Boolean =
            (charCheck str chr idx1) <> (charCheck str chr idx2)

        entries
        |> Seq.filter validEntry
        |> Seq.length

    let day = { parseFile = parseFile; part1 = part1; part2 = part2 }

module Day3 =
    let parseFile = System.IO.File.ReadAllLines

    let isTree x (line: string) =
        line.Chars(x % line.Length) = '#'

    let genPoints (dx, dy) =
        Seq.initInfinite (fun i -> (i * dx, i * dy))

    let countTrees (lines: string[]) (points: seq<int * int>) : int =
        points
        |> Seq.takeWhile (fun (_, y) -> y < lines.Length)
        |> Seq.filter (fun (x, y) -> isTree x lines.[y])
        |> Seq.length

    let part1 (lines: string[]) : int =
        (3, 1)
        |> genPoints
        |> countTrees lines

    let part2 (lines : string[]) : uint64 =
        let slopes = [
            (1, 1)
            (3, 1)
            (5, 1)
            (7, 1)
            (1, 2)
        ]

        slopes
        |> Seq.ofList
        |> Seq.map (genPoints >> (countTrees lines) >> uint64)
        |> Seq.reduce ( * )

    let day = { parseFile = parseFile; part1 = part1; part2 = part2 }

let runDay (dayNum : int) (day: Day<_, _, _>) =
  let filename = System.IO.Path.Combine(__SOURCE_DIRECTORY__, (sprintf "./inputs/%d.txt" dayNum))
  let parsed = day.parseFile filename

  printfn "Day %d Part 1 - %A" dayNum (day.part1 parsed)
  printfn "Day %d Part 2 - %A" dayNum (day.part2 parsed)

[<EntryPoint>]
let main argv =
    let dayStr = if argv.Length > 0 then argv.[0] else "1"
    
    match dayStr with
    | "1" -> runDay 1 Day1.day
    | "2" -> runDay 2 Day2.day
    | "3" -> runDay 3 Day3.day
    | _ -> failwith "Invalid day"
    0 // return an integer exit code