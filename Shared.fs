namespace Bamorim.AdventOfCode.Y2020

module Shared =
    type Day<'a, 'b, 'c> =
        { parseFile: string -> 'a
          part1: 'a -> 'b
          part2: 'a -> 'c }
