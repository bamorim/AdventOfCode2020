namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared

module Day18 =

    type Token =
        | Number of int64
        | OpenParens
        | CloseParens
        | Plus
        | Star

    let lexer (line: string): Token list =
        line
            .Replace("(", " ( ")
            .Replace(")", " ) ")
            .Replace("+", " + ")
            .Replace("*", " * ")
            .Split(" ")
        |> Seq.ofArray
        |> Seq.map (fun x -> x.Trim())
        |> Seq.choose (function
            | "(" -> Some OpenParens
            | ")" -> Some CloseParens
            | "+" -> Some Plus
            | "*" -> Some Star
            | other ->
                let success, number = Int64.TryParse other
                if success then Some(Number number) else None)
        |> List.ofSeq

    let parseFile: string -> seq<Token list> = IO.File.ReadLines >> Seq.map lexer

    type Part1Expression =
        | Value of int64
        | Operations of seq<Part1Operation>

    and Part1Operation =
        | Add of Part1Expression
        | Mul of Part1Expression

    let rec evaluatePart1 =
        function
        | Value v -> v
        | Operations ops ->
            Seq.fold (fun total ->
                function
                | Add expr -> total + (evaluatePart1 expr)
                | Mul expr -> total * (evaluatePart1 expr)) 0L ops

    let part1 (tokens: seq<Token list>): int64 =
        let parser (tokens: Token list): Part1Expression =
            let rec parseExpression tokens =
                match parseOperations [] tokens with
                | (ops, tokens) -> (Operations ops, tokens)

            and parseOperations operations =
                function
                | Plus :: tokens ->
                    let (primary, rest) = parsePrimary tokens
                    parseOperations ((Add primary) :: operations) rest
                | Star :: tokens ->
                    let (primary, rest) = parsePrimary tokens
                    parseOperations ((Mul primary) :: operations) rest
                | CloseParens :: rest -> (Seq.rev operations, CloseParens :: rest)
                | [] -> (Seq.rev operations, [])
                // Assume `+` in the beginning of expressions
                | tokens ->
                    let (primary, rest) = parsePrimary tokens
                    parseOperations ((Add primary) :: operations) rest

            and parsePrimary =
                function
                | OpenParens :: tokens ->
                    match parseExpression tokens with
                    | (expression, CloseParens :: rest) -> (expression, rest)
                    | (_, token :: rest) -> failwithf "Unexpected token %A. Expecting )" token
                    | (_, []) -> failwith "Unexpected end of string. Expecting )"
                | (Number n) :: rest -> (Value n, rest)
                | _ -> failwith "Unexpected token"

            match parseExpression tokens with
            | (expr, []) -> expr
            | (_expr, _rest) -> failwith "Unexpected tokens"

        Seq.sumBy (parser >> evaluatePart1) tokens

    type Expression =
        | Value of int64
        | Addition of Expression * Expression
        | Multiplication of Expression * Expression

    let rec evaluate =
        function
        | Value v -> v
        | Addition (a, b) -> (evaluate a) + (evaluate b)
        | Multiplication (a, b) -> (evaluate a) * (evaluate b)

    let part2 (tokens: seq<Token list>): int64 =
        let parser (tokens: Token list): Expression =
            let rec parseExpression tokens =
                match parseAddition tokens with
                | (lhs, Star :: rest) ->
                    let (rhs, rest) = parseExpression rest
                    (Multiplication(lhs, rhs), rest)
                | (mult, rest) -> (mult, rest)

            and parseAddition tokens =
                match parsePrimary tokens with
                | (lhs, Plus :: rest) ->
                    let (rhs, rest) = parseAddition rest
                    (Addition(lhs, rhs), rest)
                | (primary, rest) -> (primary, rest)

            and parsePrimary =
                function
                | OpenParens :: tokens ->
                    match parseExpression tokens with
                    | (expression, CloseParens :: rest) -> (expression, rest)
                    | (_, token :: rest) -> failwithf "Unexpected token %A. Expecting )" token
                    | (_, []) -> failwith "Unexpected end of string. Expecting )"
                | (Number n) :: rest -> (Value n, rest)
                | _ -> failwith "Unexpected token"

            match parseExpression tokens with
            | (expr, []) -> expr
            | (_expr, _rest) -> failwith "Unexpected tokens"

        Seq.sumBy (parser >> evaluate) tokens

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
