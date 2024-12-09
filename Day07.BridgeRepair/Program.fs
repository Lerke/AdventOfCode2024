open System
open System.IO

type Operation =
    | Multiply
    | Addition
    | Concat

type InputLine = { Target: Int64; Numbers: Int64 list }

let rec Combinations (input: Operation array list) =
    match input with
    | [] -> []
    | x :: xs ->
        let combinations = Combinations xs

        match combinations with
        | c when c.Length > 1 ->
            c |> List.collect (fun p -> x |> Array.toList |> List.map (fun q -> [q] @ p))
        | _ ->
            match x with
            | z when z.Length = 2 -> [ [ z[0] ]; [ z[1] ] ]
            | z when z.Length = 3 -> [ [ z[0] ]; [ z[1] ]; [ z[2] ] ]
            | _ -> failwithf "Unsupported number of operands"

let Operations input =
    seq { 0 .. (input.Numbers.Length - 2) }
    |> Seq.map (fun _ -> [| Multiply; Addition |])
    |> Seq.toList

let WithConcatOperation input =
    seq { 0 .. (input.Numbers.Length - 2) }
    |> Seq.map (fun _ -> [| Multiply; Addition; Concat |])
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
                | Concat -> fun x y -> (x.ToString() + y.ToString()) |> Int64.Parse

            let result = opFn x y
            Calculate { line with Numbers = (result :: xs) } os
        | [] -> failwithf "No operand found"
    | [ x ] -> x
    | [] -> 0

let rec ReplaceOneOperand (operands: Operation list) =
    seq { 0 .. operands.Length - 1 }
    |> Seq.map (fun f ->
        let newOps = operands |> List.toArray
        newOps[f] <- Concat
        newOps |> Array.toList)
    |> Seq.toList

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

        let successCalculations =
            possibleCalculations
            |> Seq.filter (fun (l, results) -> results |> Seq.contains l.Target)
            |> Seq.toArray

        let oneStar = successCalculations |> Seq.sumBy (fun (l, _) -> l.Target)

        printfn $"⭐\tResult:\t%A{oneStar}"

        let failingCalculations =
            possibleCalculations
            |> Seq.filter (fun (l, results) -> results |> Seq.contains l.Target |> not)
            |> Seq.toArray
            
        let replacedCalculations =
            failingCalculations
            |> Seq.map fst
            |> Seq.map WithConcatOperation
            |> Seq.map Combinations
            |> Seq.zip failingCalculations
            |> Seq.map (fun (l, o) -> (l, o |> (Seq.map (Calculate (l |> fst)))))
            |> Seq.filter (fun (l, results) -> results |> Seq.contains (l |> fst).Target)
            |> Seq.toArray

        let twoStar =
            (successCalculations |> Seq.sumBy (fun (l, _) -> l.Target))
            + (replacedCalculations |> Seq.sumBy (fun (l, _) -> (l |> fst).Target))

        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
