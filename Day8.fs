namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day8 =
    type Instruction =
        | Jmp of int
        | Acc of int
        | Noop of int

    type ProgramState =
        { Index: int
          Accumulator: int
          Executed: Set<int> }

    let parseFile (filename: string): Instruction array =
        let parseOperation (line: string): (int -> Instruction) =
            match line.Substring(0, 3) with
            | "jmp" -> Jmp
            | "acc" -> Acc
            | "nop" -> Noop
            | _ -> failwith "invalid operation"

        let parseValue (line: string): int = int (line.Substring 4)

        let parseInstruction (line: string): Instruction = (parseOperation line) (parseValue line)

        filename
        |> IO.File.ReadLines
        |> Seq.map parseInstruction
        |> Array.ofSeq

    let initialState: ProgramState =
        { Index = 0
          Accumulator = 0
          Executed = Set.empty }

    let runInstruction state callback inst =
        let newState =
            { state with
                  Executed = Set.add state.Index state.Executed }

        match inst with
        | Jmp diff ->
            callback
                { newState with
                      Index = state.Index + diff }
        | Acc diff ->
            callback
                { newState with
                      Index = state.Index + 1
                      Accumulator = state.Accumulator + diff }
        | Noop _ ->
            callback
                { newState with
                      Index = state.Index + 1 }

    let part1 (instructions: Instruction array): int =
        let rec aux state =
            if Set.contains state.Index state.Executed
            then state.Accumulator
            else runInstruction state aux instructions.[state.Index]

        aux initialState

    let part2 (instructions: Instruction array): int =
        let tryRun (repl_i: int, replacement: Instruction) =
            let rec aux state =
                if Set.contains state.Index state.Executed then
                    None
                elif state.Index >= instructions.Length then
                    Some state.Accumulator
                else
                    if state.Index = repl_i then replacement else instructions.[state.Index]
                    |> runInstruction state aux

            aux initialState

        // This generates a sequence of possible replacements and runs
        // the program with each until it finds one that terminates
        instructions
        |> Seq.ofArray
        |> Seq.mapi (fun i ->
            function
            | (Jmp diff) -> Some(i, Noop diff)
            | (Noop diff) -> Some(i, Jmp diff)
            | _ -> None)
        |> Seq.choose id
        |> Seq.pick tryRun

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
