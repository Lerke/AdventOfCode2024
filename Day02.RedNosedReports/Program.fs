open System
open System.IO

type Report(levels: int array) =
    member val Levels = levels

    member this.IsSafe() =
        let steps = levels |> Seq.pairwise |> Seq.map (fun (a, b) -> a - b)

        (steps |> Seq.forall (fun f -> f > 0) || steps |> Seq.forall (fun f -> f < 0))
        && (steps |> Seq.forall (fun f -> (f |> abs) >= 1 && (f |> abs) <= 3))


match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 02: Red Nosed Reports ---"

        let input =
            File.ReadAllLines file
            |> Seq.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
            |> Seq.map _.Split(" ")
            |> Seq.map (fun f -> f |> Array.map Int32.Parse)
            |> Seq.map Report
            
        let safeReports =
            input
            |> Seq.filter(_.IsSafe())

        printfn $"⭐\tSafe Reports:\t\t%A{safeReports |> Seq.length}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
