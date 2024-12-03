open System
open System.IO
open System.Text.RegularExpressions

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 03: Mull It Over ---"
        let input = File.ReadAllText file

        let matches =
            Regex.Matches(input, @"(mul\((?<op1>\d+),(?<op2>\d+)\))|(do(?:n't)?\(\))")

        let multiplications =
            matches
            |> Seq.filter (fun f -> f.Groups["op1"].Success && f.Groups["op2"].Success)
            |> Seq.map (fun f ->
                (f.Index, f.Groups["op1"].Value |> Int32.Parse, (f.Groups["op2"].Value |> Int32.Parse)))

        let oneStar = multiplications |> Seq.sumBy (fun (_, p, q) -> p * q)
        printfn $"⭐\tResult:\t%A{oneStar}"

        let onOffRanges =
            matches
            |> Seq.filter (fun f -> f.Groups["op1"].Success = false && f.Groups["op2"].Success = false)
            |> Seq.map (fun f -> (f.Index, f.Value = "do()"))
            |> (fun f -> f |> Seq.append (seq { (0, true) }))
            |> Seq.pairwise

        let twoStar =
            multiplications
            |> Seq.filter (fun (i, _, _) ->
                match
                    (onOffRanges
                     |> Seq.tryFind (fun ((rStart, _), (rEnd, _)) -> rStart <= i && i <= rEnd))
                with
                | None -> ((onOffRanges |> Seq.last) |> snd |> snd)
                | Some value -> value |> fst |> snd)
            |> Seq.sumBy (fun (_, p, q) -> p * q)

        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
