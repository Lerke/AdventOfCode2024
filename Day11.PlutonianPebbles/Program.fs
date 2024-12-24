﻿open System
open System.IO
open Microsoft.FSharp.Core

let ApplyRules (number: int64) (maxDepth: int64) =
    let rec _apply (number: int64) (depth: int64) (maxDepth: int64) =
        match depth with
        | x when x >= maxDepth -> 1
        | _ ->
            match number with
            | x when x = 0 -> (_apply 1L (depth + 1L) maxDepth)
            | x when x.ToString().Length % 2 = 0 ->
                let l = x.ToString().Length
                let c1 = x.ToString().Substring(0, l / 2) |> Int64.Parse
                let c2 = x.ToString().Substring(l / 2) |> Int64.Parse
                (_apply c1 (depth + 1L) maxDepth) + (_apply c2 (depth + 1L) maxDepth)
            | x -> (_apply (x * 2024L) (depth + 1L) maxDepth)

    (_apply number 0 maxDepth)
    
match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 11: Plutonian Pebbles ---"
        let input = (File.ReadAllText file).Split(" ") |> Seq.map Int32.Parse |> Seq.toArray
        let oneStar =
            input
            |> Array.map (fun f -> ApplyRules f 25)
            |> Array.sum
        let twoStar = 0

        printfn $"⭐\tResult:\t%A{oneStar}"
        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore