open System
open System.IO

type XmasBoard(Board) =
    member val private Board: char array array = Board

    member this.Index(x, y) =
        match ((y < this.Board.Length && y >= 0), (x < this.Board[0].Length && x >= 0)) with
        | true, true -> Some(this.Board[y][x])
        | _ -> None

    member this.Diagonals(x, y) =
        let sequences =
            seq {
                (fun x y i -> (x + i, y + 0))
                (fun x y i -> (x + 0, y + i))
                (fun x y i -> (x - i, y + 0))
                (fun x y i -> (x + 0, y - i))
                (fun x y i -> (x + i, y + i))
                (fun x y i -> (x + i, y - i))
                (fun x y i -> (x - i, y + i))
                (fun x y i -> (x - i, y - i))
            }

        sequences
        |> Seq.map (fun fn -> (seq { 0 .. "XMAS".Length - 1 } |> Seq.map (fn x y) |> Seq.map this.Index))
        |> Seq.filter (fun f -> f |> Seq.forall _.IsSome)
        |> Seq.map (fun f -> (f |> Seq.map _.Value))

    member this.XmasInBoard() =
        seq { 0 .. this.Board.Length - 1 }
        |> Seq.collect (fun f -> (seq { 0 .. this.Board[0].Length - 1 }) |> Seq.map (fun p -> (f, p)))
        |> Seq.collect this.Diagonals
        |> Seq.filter (fun f -> f |> Seq.rev |> String.Concat = "XMAS")

    member this.XDashMasInBoard() =
        let isMas x =
            x = (Some 'M', Some 'A', Some 'S') || x = (Some 'S', Some 'A', Some 'M')

        seq { 0 .. this.Board.Length - 1 }
        |> Seq.collect (fun f -> (seq { 0 .. this.Board[0].Length - 1 }) |> Seq.map (fun p -> (f, p)))
        |> Seq.filter (fun (y, x) -> this.Index(x, y) = Some 'A')
        |> Seq.map (fun (y, x) ->
            [| this.Index(x - 1, y - 1), this.Index(x, y), this.Index(x + 1, y + 1)
               this.Index(x - 1, y + 1), this.Index(x, y), this.Index(x + 1, y - 1) |])
        |> Seq.filter (fun w -> isMas w[0] && isMas w[1])

    new(lines: string array) = XmasBoard(lines |> Array.map _.ToCharArray())

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 04: Ceres Search ---"
        let input = File.ReadAllLines file
        let board = XmasBoard input
        let resultOne = board.XmasInBoard >> Seq.length
        let resultTwo = board.XDashMasInBoard >> Seq.length

        printfn $"⭐\tResult:\t%A{resultOne ()}"
        printfn $"⭐⭐\tResult:\t%A{resultTwo ()}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
