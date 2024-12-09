open System
open System.IO

type Operation =
    | Multiply
    | Addition

type InputLine = { Target: Int64; Numbers: Int64 list }

let rec Combinations (input: Operation array list) =
    match input with
    | [] -> []
    | x :: xs ->
        let combinations = Combinations xs

        match combinations with
        | c when c.Length > 1 -> (c |> List.collect (fun p -> [ [ x[0] ] @ p; [ x[1] ] @ p ]))
        | _ -> [ [ x[0] ]; [ x[1] ] ]

let Operations input =
    seq { 0 .. (input.Numbers.Length - 2) }
    |> Seq.map (fun _ -> [| Multiply; Addition |])
    |> Seq.toList

let rec Calculate (line: InputLine) (operands: Operation list) =
    match line.Numbers with
    | x :: y :: xs ->
        match operands with
        | op :: os ->
            let opFn =
                match op with
                | Multiply -> (*)
                | Addition -> (+)

            let result = opFn x y
            Calculate { line with Numbers = (result :: xs) } os
        | [] -> failwithf "No operand found"
    | [ x ] -> x
    | [] -> 0

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 07: Bridge Repair ---"
        let input = File.ReadAllLines file

        let lines =
            input
            |> Seq.map (fun f ->
                let split = f.Split(":")
                let target = split[0] |> Int64.Parse
                let numbers = split[1].Trim().Split(" ") |> Seq.map Int64.Parse |> Seq.toList
                { Target = target; Numbers = numbers })
            |> Seq.toArray

        let possibleCalculations =
            lines
            |> Seq.map Operations
            |> Seq.map Combinations
            |> Seq.zip lines
            |> Seq.map (fun (l, o) -> (l, o |> (Seq.map (Calculate l))))
            |> Seq.filter (fun (l, results) -> results |> Seq.contains l.Target)
            |> Seq.toArray

        let oneStar = possibleCalculations |> Seq.sumBy (fun (l, _) -> l.Target)

        printfn $"⭐\tResult:\t%A{oneStar}"
        printfn $"⭐⭐\tResult:\t%A{'?'}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
