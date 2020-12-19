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

    type Expression =
        | Value of int64
        | Addition of Expression * Expression
        | Multiplication of Expression * Expression

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

    let rec evaluate =
        function
        | Value v -> v
        | Addition (a, b) -> (evaluate a) + (evaluate b)
        | Multiplication (a, b) -> (evaluate a) * (evaluate b)

    // Parses something that looks like a value (either a raw value or a sub-expression)
    let parsePrimary parseExpression =
        function
        | OpenParens :: tokens ->
            match parseExpression tokens with
            | (expression, CloseParens :: rest) -> (expression, rest)
            | (_, token :: rest) -> failwithf "Unexpected token %A. Expecting )" token
            | (_, []) -> failwith "Unexpected end of string. Expecting )"
        | (Number n) :: rest -> (Value n, rest)
        | _ -> failwith "Unexpected token"

    let makeParser parseExpression tokens =
        match parseExpression tokens with
        | (expr, []) -> expr
        | (_expr, _rest) -> failwith "Unexpected tokens"


    let part1 (tokens: seq<Token list>): int64 =
        // Reverse the expression to make left-to-right evaluation simpler
        let revertExpression tokens =
            tokens
            |> List.rev
            |> List.map (function
                | CloseParens -> OpenParens
                | OpenParens -> CloseParens
                | token -> token)

        let rec parseExpression tokens =
            match parsePrimary parseExpression tokens with
            | (lhs, Star :: rest) ->
                let (rhs, rest) = parseExpression rest
                (Multiplication(lhs, rhs), rest)
            | (lhs, Plus :: rest) ->
                let (rhs, rest) = parseExpression rest
                (Addition(lhs, rhs), rest)
            | (mult, rest) -> (mult, rest)

        let parser =
            revertExpression >> (makeParser parseExpression)

        Seq.sumBy (parser >> evaluate) tokens


    let part2 (tokens: seq<Token list>): int64 =
        let rec parseExpression tokens =
            match parseAddition tokens with
            | (lhs, Star :: rest) ->
                let (rhs, rest) = parseExpression rest
                (Multiplication(lhs, rhs), rest)
            | (mult, rest) -> (mult, rest)

        and parseAddition tokens =
            match parsePrimary parseExpression tokens with
            | (lhs, Plus :: rest) ->
                let (rhs, rest) = parseAddition rest
                (Addition(lhs, rhs), rest)
            | (primary, rest) -> (primary, rest)

        let parser = makeParser parseExpression

        Seq.sumBy (parser >> evaluate) tokens

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = part1
          part2 = part2 }
