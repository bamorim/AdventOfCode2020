open System

type Day<'a, 'b, 'c> = { num: int; parseFile: string -> 'a; part1: 'a -> 'b; part2: 'a -> 'c}

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

    let day = { num = 1; parseFile = parseFile; part1 = part1; part2 = part2 }

let runDay (day: Day<_, _, _>) =
  let filename = System.IO.Path.Combine(__SOURCE_DIRECTORY__, (sprintf "./inputs/%d.txt" day.num))
  let parsed = day.parseFile filename

  printfn "Day %d Part 1 - %d" day.num (day.part1 parsed)
  printfn "Day %d Part 2 - %d" day.num (day.part2 parsed)

[<EntryPoint>]
let main argv =
    runDay Day1.day

    0 // return an integer exit code