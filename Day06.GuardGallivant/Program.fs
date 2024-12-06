open System
open System.IO
open Microsoft.FSharp.Collections

type MapElement =
    | Free
    | Obstacle

type Direction =
    | North
    | East
    | South
    | West

type Position = int * int

type Guard = Position * Direction

let RotateDirection direction =
    match direction with
    | North -> East
    | East -> South
    | South -> West
    | West -> North

let NextDirection direction =
    match direction with
    | North -> (0, -1)
    | East -> (1, 0)
    | South -> (0, 1)
    | West -> (-1, 0)

let (+>>) ((px, py): Position) (direction: Direction) =
    (NextDirection direction)
    |> fun (nextX, nextY) -> (px + nextX), (py + nextY)

let (+>) ((p1x, p1y): Position) ((p2x, p2y): Position) = (p1x + p2x), (p1y + p2y)

type Map(Board, Guard) =
    member val Board: MapElement array array = Board
    member val Guard: Guard = Guard

    member this.Print() =
        let map =
            this.Board
            |> Array.map (fun f ->
                f
                |> Array.map (fun p ->
                    match p with
                    | Free -> "."
                    | Obstacle -> "#"))

        let (guardX, guardY), direction = this.Guard

        let guardIcon =
            match direction with
            | North -> "^"
            | East -> ">"
            | South -> "v"
            | West -> "<"

        map[guardY].[guardX] <- guardIcon

        String.Join("\n", map |> Seq.map (fun f -> String.Join("", f)))

    member this.ProcessMove() =
        let moveDirection =
            let guardPos, guardDirection = Guard
            let nextX, nextY = guardPos +>> guardDirection

            match (Array.tryItem nextY Board) with
            | Some row ->
                match (Array.tryItem nextX row) with
                | Some _ -> Some this.Board[nextY].[nextX]
                | None -> None
            | None -> None

        match moveDirection with
        | Some value ->
            match value with
            | Free -> Some(Map(this.Board, ((Guard |> fst) +> NextDirection(Guard |> snd), (Guard |> snd))))
            | Obstacle -> Some(Map(this.Board, ((Guard |> fst), (Guard |> snd) |> RotateDirection)))
        | None -> None

    new(input: String array) =
        let board =
            input
            |> Seq.map (
                fun f ->
                    f.ToCharArray()
                    |> Seq.map (fun s ->
                        match s with
                        | '.' -> Free
                        | '^' -> Free
                        | '#' -> Obstacle
                        | e -> failwithf $"Unknown symbol %A{e}")
                >> Seq.toArray
            )
            |> Seq.toArray

        let guardPos =
            match (input |> Seq.collect id) |> (Seq.tryFindIndex ((=) '^')) with
            | Some pos ->
                let len = input[0] |> Seq.length
                (pos % len, pos / len)
            | None -> failwithf "Could not find guard position"

        Map(board, Guard(guardPos, North))

let rec CalculateUntilGuardLeaves (map: Map) =
    let guardPos = map.Guard

    match map.ProcessMove() with
    | None -> [ guardPos ]
    | Some value -> [ guardPos ] @ (CalculateUntilGuardLeaves value)

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 05: Print Queue ---"
        let input = File.ReadAllLines file
        let board = Map(input)
        let positions = CalculateUntilGuardLeaves board
        let distinctPositions = positions |> Seq.distinctBy fst |> Seq.length

        printfn $"⭐\tResult:\t%A{distinctPositions}"
        // printfn $"⭐⭐\tResult:\t%A{resultTwo ()}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
