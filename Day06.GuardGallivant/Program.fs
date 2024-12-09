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
    (NextDirection direction) |> fun (nextX, nextY) -> (px + nextX), (py + nextY)

let (+>) ((p1x, p1y): Position) ((p2x, p2y): Position) = (p1x + p2x), (p1y + p2y)

type Map(Board, Guard) =
    member val Board: MapElement array array = Board
    member val Guard: Guard = Guard

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
    match map.ProcessMove() with
    | None -> [ map.Guard ]
    | Some value -> [ map.Guard ] @ (CalculateUntilGuardLeaves value)

let rec CalculateUntilGuardLeavesOrLoops (map: Map, path: Guard list) =
    match (path |> List.contains map.Guard) with
    | true -> Some map
    | false ->
        match map.ProcessMove() with
        | None -> None
        | Some value -> CalculateUntilGuardLeavesOrLoops(value, [ map.Guard ] @ path)

let CalculatePossibleObstaclePlaces (map: Map) =
    let (gx, gy), gd = map.Guard

    let positions = CalculateUntilGuardLeaves map |> Seq.distinctBy fst |> Seq.toArray

    positions
    |> Seq.collect (fun ((x, y), d) ->
        [ ((x, y), d)
          ((x + 1, y), d)
          ((x + 1, y + 1), d)
          ((x + 1, y - 1), d)
          ((x, y - 1), d)
          ((x, y + 1), d)
          ((x - 1, y - 1), d)
          ((x - 1, y + 1), d)
          ((x - 1, y), d) ])
    |> Seq.distinctBy fst
    |> Seq.map (fun ((x, y), _) ->
        let newMap =
            Map(map.Board |> Array.map (fun f -> f |> Array.map id), new Guard((gx, gy), gd))

        match (newMap.Board |> Array.tryItem y) with
        | Some s ->
            match (s |> Array.tryItem x) with
            | Some i when i <> Obstacle ->
                newMap.Board[y].[x] <- Obstacle
                Some newMap
            | _ -> None
        | None -> None)
    |> Seq.filter _.IsSome
    |> Seq.toArray
    |> Array.Parallel.map (fun f -> CalculateUntilGuardLeavesOrLoops(f.Value, []))
    |> Seq.filter _.IsSome
    |> Seq.map (_.Value)

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 06: Guard Gallivant ---"
        let input = File.ReadAllLines file
        let board = Map(input)
        let positions = CalculateUntilGuardLeaves board
        let distinctPositions = positions |> Seq.distinctBy fst |> Seq.length

        printfn $"⭐\tResult:\t%A{distinctPositions}"

        let newObstaclePositions = CalculatePossibleObstaclePlaces(Map(input)) |> Seq.length
        printfn $"⭐⭐\tResult:\t%A{newObstaclePositions}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
