open System
open System.IO

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 01: Historian Hysteria ---"
        let input =
            File.ReadAllLines file
            |> Seq.filter (fun f -> String.IsNullOrWhiteSpace(f) = false)
            |> Seq.map (fun f -> f.Split(" ") |> Array.filter (fun f -> String.IsNullOrWhiteSpace(f) = false))
            |> Seq.map (fun f -> (f[0] |> Int32.Parse, f[1] |> Int32.Parse))
            |> Seq.fold (fun acc (l1, l2) -> ((acc |> fst) @ [ l1 ], (acc |> snd) @ [ l2 ])) ([], [])

        let differences =
            input
            |> fun f -> (f |> snd |> List.sort) |> List.zip (f |> fst |> List.sort)
            |> List.map (fun (p, q) ->
                {| ValueOne = q
                   ValueTwo = p
                   Difference = (p - q) |> abs |})

        let totalDistance = differences |> List.sumBy _.Difference
        printfn $"⭐\tTotal Distance:\t\t%A{totalDistance}"

        let occurenceCount =
            input
            |> snd
            |> fun l2 ->
                (l2 |> List.groupBy id |> List.map (fun f -> (f |> fst, f |> snd |> List.length)))
                |> dict

        let similarityScore =
            (input |> fst)
            |> List.sumBy (fun f ->
                match occurenceCount.TryGetValue f with
                | true, o -> f * o
                | false, _ -> 0)

        printfn $"⭐⭐\tSimilarity Score:\t%A{similarityScore}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
