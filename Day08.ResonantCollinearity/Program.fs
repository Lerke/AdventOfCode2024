open System
open System.IO

type Position = int * int
type Antenna = { Frequency: char; Position: Position }

type Mode =
    | OneStar
    | TwoStar

let (--) (p1: Position) (p2: Position) =
    let p1x, p1y = p1
    let p2x, p2y = p2
    (p1x - p2x), (p1y - p2y)

let (++) (p1: Position) (p2: Position) =
    let p1x, p1y = p1
    let p2x, p2y = p2
    (p1x + p2x), (p1y + p2y)

let ParseInput (input: string array) =
    input
    |> Seq.indexed
    |> Seq.collect (fun (y, str) -> str.ToCharArray() |> Seq.indexed |> Seq.map (fun (x, c) -> (x, y, c)))

let Antinodes (a1: Antenna) (a2: Antenna) (mode: Mode) =
    let dx, dy = a2.Position -- a1.Position

    let rec generator n =
        seq {
            yield (a1.Position -- (dx * n, dy * n)), (a2.Position ++ (dx * n, dy * n))
            yield! generator (n + 1)
        }

    match mode with
    | OneStar -> generator 1
    | TwoStar -> generator 0

let InBounds (xMax: int) (yMax: int) ((px, py): Position) =
    (px >= 0 && px <= xMax) && (py >= 0 && py <= yMax)

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 08: Resonant Collinearity ---"
        let input = File.ReadAllLines file
        let map = ParseInput input
        let isInBoundsOfMap = InBounds (input[0].Length - 1) (input.Length - 1)

        let antennas =
            map
            |> Seq.filter (fun (_, _, c) -> c <> '.')
            |> Seq.map (fun (x, y, c) ->
                { Position = Position(x, y)
                  Frequency = c })
            |> Seq.toList

        let allAntinodes mode =
            antennas
            |> Seq.groupBy _.Frequency
            |> Seq.map (fun (_, antennas) -> Seq.allPairs antennas antennas)
            |> Seq.collect id
            |> Seq.filter (fun (a1, a2) -> a1 <> a2)
            |> Seq.map (fun (a1, a2) ->
                Antinodes a1 a2 mode
                |> Seq.takeWhile (fun (a1, a2) -> a1 |> isInBoundsOfMap || a2 |> isInBoundsOfMap))
            |> Seq.collect (fun f ->
                match mode with
                | OneStar -> seq { Seq.tryHead f }
                | TwoStar -> f |> Seq.map Some)
            |> Seq.filter _.IsSome
            |> Seq.map (fun f -> f.Value |> fst)
            |> Seq.filter isInBoundsOfMap
            |> Seq.distinct

        let oneStar = OneStar |> allAntinodes |> Seq.length
        let twoStar = TwoStar |> allAntinodes |> Seq.length
        printfn $"⭐\tResult:\t%A{oneStar}"
        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
