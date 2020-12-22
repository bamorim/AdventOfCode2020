namespace Bamorim.AdventOfCode.Y2020

open System
open Bamorim.AdventOfCode.Y2020.Shared
open System.Text.RegularExpressions

module Day20 =
    type TileId = int
    type RawTile = char [,]

    type Rotation =
        | Zero
        | Ninety
        | OneEighty
        | TwoSeventy

    type Flip =
        | NotFlipped
        | Flipped

    let (|TileHeader|_|) input =
        let m = Regex.Match(input, "^Tile (\d+):$")
        if m.Success then Some(int m.Groups.[1].Value) else None

    let parseFile (filename: string): seq<TileId * RawTile> =
        filename
        |> IO.File.ReadAllText
        |> (fun all -> all.Trim().Split("\n\n"))
        |> Seq.map (fun tileString ->
            let lines = tileString.Split("\n")

            match lines.[0] with
            | TileHeader tileId ->
                let rawTile =
                    lines
                    |> Array.skip 1
                    |> Array.map (fun line -> line.Trim().ToCharArray())
                    |> array2D

                (tileId, rawTile)
            | _ -> failwith "invalid tile")

    let getGridSize tiles =
        tiles |> Seq.length |> float |> sqrt |> int

    module Part1 =
        type Side =
            | Top
            | Right
            | Bottom
            | Left

        type Position = int * int
        type PlacedTile = Flip * Rotation * TileId
        type Solution = Map<Position, PlacedTile>

        // EdgeId is the number by assuming a binary representation
        // of traversing an edge where `.` is 0 and `#` is 1 in either
        // a clock-wise direction or counter-clock-wise
        type Edge = String
        // Original Top/Right/Bottom/Left
        type TileEdges = Edge * Edge * Edge * Edge

        let reverse (edge: Edge): Edge =
            edge.ToCharArray() |> Array.rev |> String

        let transform (flip: Flip) (rotation: Rotation): TileEdges -> TileEdges =
            let flipHorizontally (original: TileEdges): TileEdges =
                let t, r, b, l = original
                (reverse t), (reverse l), (reverse b), (reverse r)

            let rotate90 (original: TileEdges) =
                let t, r, b, l = original
                l, t, r, b

            let rotate =
                function
                | Zero -> id
                | Ninety -> rotate90
                | OneEighty -> rotate90 >> rotate90
                | TwoSeventy -> rotate90 >> rotate90 >> rotate90

            match flip with
            | NotFlipped -> rotate rotation
            | Flipped -> flipHorizontally >> rotate rotation

        let rawTileToTileEdges (tile: RawTile): TileEdges =
            (String tile.[0, *], String tile.[*, 9], String(Array.rev tile.[9, *]), String(Array.rev tile.[*, 0]))

        let genTransformations (original: TileEdges): seq<Flip * Rotation * TileEdges> =
            seq {
                for flip in [ Flipped; NotFlipped ] do
                    for rotation in [ Zero; Ninety; OneEighty; TwoSeventy ] do
                        yield flip, rotation, transform flip rotation original
            }

        let findSolution (rawTiles: seq<TileId * RawTile>) =
            let tiles =
                rawTiles
                |> Seq.map (fun (id, rawTile) -> (id, rawTileToTileEdges rawTile))
                |> Map.ofSeq

            let getSolutionTile pos solution =
                let (flip, rotation, tileId) = Map.find pos solution

                tiles
                |> Map.find tileId
                |> transform flip rotation

            let tileIdsByEdge: Map<Side * Edge, Set<PlacedTile>> =
                seq {
                    for (tileId, edges) in Map.toSeq tiles do
                        for transformation in genTransformations edges do
                            let flip, rotation, transformed = transformation
                            let t, _, _, l = transformed
                            yield (Top, t), (flip, rotation, tileId)
                            yield (Left, l), (flip, rotation, tileId)
                }
                |> Seq.groupBy fst
                |> Seq.map (fun (edge, x) -> (edge, x |> Seq.map snd |> Set.ofSeq))
                |> Map.ofSeq

            let tileCombinations =
                seq {
                    for (id, tile) in (Map.toSeq tiles) do
                        for (flip, rotation, _) in genTransformations tile do
                            yield (flip, rotation, id)
                }

            let initialSolutions =
                Seq.map (fun c -> Map.add (0, 0) c Map.empty) tileCombinations

            let maxXY = (getGridSize rawTiles) - 1

            let nextPos (x, y) =
                if x < (maxXY) then Some(x + 1, y)
                elif y < (maxXY) then Some(0, y + 1)
                else None

            let rec doFindSolution ((x, y): Position) (solution: Solution): Solution option =
                let constraints: seq<Side * Edge> =
                    seq {
                        if x > 0 then
                            let _, r, _, _ = getSolutionTile (x - 1, y) solution
                            yield (Left, reverse r)

                        if y > 0 then
                            let _, _, b, _ = getSolutionTile (x, y - 1) solution
                            yield (Top, reverse b)
                    }

                let usedTiles =
                    solution
                    |> Map.toSeq
                    |> Seq.map (fun (_, (_, _, id)) -> id)
                    |> Set.ofSeq

                let possibilities: Set<PlacedTile> =
                    seq {
                        for consts in constraints do
                            tileIdsByEdge
                            |> Map.tryFind consts
                            |> Option.defaultValue Set.empty
                    }
                    |> Seq.reduce Set.intersect
                    |> Set.filter (fun (_, _, id) -> not <| Set.contains id usedTiles)

                let nextSolutions: seq<Solution> =
                    Seq.map (fun tileId -> Map.add (x, y) tileId solution) possibilities

                match nextPos (x, y) with
                | Some pos -> Seq.tryPick (doFindSolution pos) nextSolutions
                | None -> Seq.tryHead nextSolutions

            Seq.pick (doFindSolution (1, 0)) initialSolutions

        let part1 (rawTiles: seq<TileId * RawTile>): uint64 =
            let solution = findSolution rawTiles

            let maxXY = (getGridSize rawTiles) - 1

            let _, _, topLeft = Map.find (0, 0) solution
            let _, _, topRight = Map.find (0, maxXY) solution
            let _, _, bottomLeft = Map.find (maxXY, 0) solution
            let _, _, bottomRight = Map.find (maxXY, maxXY) solution

            (uint64 topLeft)
            * (uint64 topRight)
            * (uint64 bottomLeft)
            * (uint64 bottomRight)

    module Part2 =
        type Pixel =
            | Hash
            | Dot
            | O

        type Pattern = (Pixel option) [,]

        let transform (flip: Flip) (rotation: Rotation): 'a [,] -> 'a [,] =
            let flipHorizontally (original: 'a [,]): 'a [,] =
                let height, width =
                    Array2D.length1 original, Array2D.length2 original

                Array2D.init height width (fun row column -> Array2D.get original row (width - column - 1))

            let rotate90 (original: 'a [,]): 'a [,] =
                let height, width =
                    Array2D.length1 original, Array2D.length2 original

                Array2D.init width height (fun row column -> Array2D.get original (height - column - 1) row)

            let rotate =
                function
                | Zero -> id
                | Ninety -> rotate90
                | OneEighty -> rotate90 >> rotate90
                | TwoSeventy -> rotate90 >> rotate90 >> rotate90

            match flip with
            | NotFlipped -> rotate rotation
            | Flipped -> flipHorizontally >> rotate rotation

        let cropTile (tile: RawTile): RawTile =
            let height, width =
                Array2D.length1 tile, Array2D.length2 tile

            Array2D.init (height - 2) (width - 2) (fun row column -> Array2D.get tile (row + 1) (column + 1))

        let tryCharToPixel =
            function
            | '#' -> Some Hash
            | '.' -> Some Dot
            | 'O' -> Some O
            | _ -> None

        let monsterPattern: Pattern =
            let pattern =
                [ "                  # "
                  "#    ##    ##    ###"
                  " #  #  #  #  #  #   " ]

            pattern
            |> Seq.map (fun line -> Array.map tryCharToPixel <| line.ToCharArray())
            |> array2D

        let matchPattern (pattern: Pattern) ((drow, dcol): int * int) (image: Pixel [,]): Boolean =
            let height, width =
                Array2D.length1 pattern, Array2D.length2 pattern

            seq {
                for row in 0 .. height - 1 do
                    for col in 0 .. width - 1 do
                        match pattern.[row, col] with
                        | Some pixel -> yield (row, col, pixel)
                        | _ -> ()
            }
            |> Seq.forall (fun (row, col, pixel) -> image.[row + drow, col + dcol] = pixel)

        let applyPattern (pattern: Pattern) ((drow, dcol): int * int): Pixel [,] -> Pixel [,] =
            let height, width =
                Array2D.length1 pattern, Array2D.length2 pattern

            Array2D.mapi (fun row col original ->
                if (row >= drow && row - drow < height)
                   && (col >= dcol && col - dcol < width) then
                    match pattern.[row - drow, col - dcol] with
                    | Some _ -> O
                    | None -> original
                else
                    original)

        let applyPatternEntireImage (pattern: Pattern) (image: Pixel [,]): Pixel [,] =
            let pHeight, pWidth =
                Array2D.length1 pattern, Array2D.length2 pattern

            let iHeight, iWidth =
                Array2D.length1 image, Array2D.length2 image

            let offsets =
                seq {
                    for row in 0 .. iHeight - pHeight - 1 do
                        for col in 0 .. iWidth - pWidth - 1 do
                            yield (row, col)
                }

            Seq.fold (fun image offset ->
                if matchPattern pattern offset image then applyPattern pattern offset image else image) image offsets

        let roughness (image: Pixel [,]) =
            let height, width =
                Array2D.length1 image, Array2D.length2 image

            Seq.length
            <| seq {
                for row in 0 .. height - 1 do
                    for col in 0 .. width - 1 do
                        match image.[row, col] with
                        | Hash -> yield ()
                        | _ -> ()
               }

        let part2 (rawTilesS: seq<TileId * RawTile>): int =
            let rawTiles = Map.ofSeq rawTilesS
            let collation = Part1.findSolution rawTilesS
            let gridSize = getGridSize rawTilesS
            let imageSize = gridSize * 8

            let tiles: RawTile [,] =
                Array2D.init gridSize gridSize (fun row col ->
                    let (flip, rotation, tileId) = Map.find (col, row) collation

                    rawTiles
                    |> Map.find tileId
                    |> transform flip rotation)

            let image =
                Array2D.init imageSize imageSize (fun i j ->
                    let row' = i / 8
                    let row = (i % 8) + 1
                    let col' = j / 8
                    let col = (j % 8) + 1

                    tiles.[row', col'].[row, col]
                    |> tryCharToPixel
                    |> Option.get)

            let images =
                seq {
                    for flip in [ Flipped; NotFlipped ] do
                        for rotation in [ Zero; Ninety; OneEighty; TwoSeventy ] do
                            yield transform flip rotation image
                }

            images
            |> Seq.map
                ((applyPatternEntireImage monsterPattern)
                 >> roughness)
            |> Seq.min

    let day: Day<_, _, _> =
        { parseFile = parseFile
          part1 = Part1.part1
          part2 = Part2.part2 }
