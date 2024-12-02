open System
open System.IO

type Dampening =
    | Dampened
    | Undampened

type Report(levels: int list) =
    member val Levels = levels

    member this.IsSafe(dampened: Dampening) =
        let steps l =
            l |> Seq.pairwise |> Seq.map (fun (a, b) -> b - a)

        let safe x =
            (x |> Seq.forall (fun f -> f > 0) || x |> Seq.forall (fun f -> f < 0))
            && (x |> Seq.forall (fun f -> (f |> abs) >= 1 && (f |> abs) <= 3))

        let except i a =
            a |> Seq.skip (i + 1) |> Seq.append (a |> Seq.take i)

        match (this.Levels |> steps |> safe) with
        | true -> true
        | false ->
            match dampened with
            | Undampened -> false
            | Dampened ->
                levels
                |> Seq.indexed
                |> Seq.map (fun (i, _) -> levels |> (except i))
                |> Seq.map (fun f -> f |> steps |> safe)
                |> Seq.contains true


match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 02: Red Nosed Reports ---"

        let input =
            File.ReadAllLines file
            |> Seq.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
            |> Seq.map _.Split(" ")
            |> Seq.map (fun f -> f |> Array.map Int32.Parse |> Array.toList)
            |> Seq.map Report

        printfn $"⭐\tSafe Reports:\t\t\t%A{(input |> Seq.filter _.IsSafe(Undampened)) |> Seq.length}"
        printfn $"⭐⭐\tSafe Reports (Dampened):\t%A{(input |> Seq.filter _.IsSafe(Dampened)) |> Seq.length}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
