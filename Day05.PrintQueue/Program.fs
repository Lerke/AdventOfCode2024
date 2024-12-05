open System
open System.IO

type Rule = { Lower: int; Upper: int }

let InputMatchesRules (input: int array) (rules: Rule array) =
    seq { 0 .. input.Length - 1 }
    |> Seq.map (fun f ->
        let beforeRules = rules |> Array.filter (fun r -> r.Upper = input[f])
        let afterRules = rules |> Array.filter (fun r -> r.Lower = input[f])

        beforeRules
        |> Seq.map _.Lower
        |> Seq.forall (fun r -> (input |> Seq.contains r |> not) || input |> Seq.take f |> Seq.contains r)
        && afterRules
           |> Seq.map _.Upper
           |> Seq.forall (fun r -> (input |> Seq.contains r |> not) || input |> Seq.skip f |> Seq.contains r))
    |> Seq.forall ((=) true)

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 05: Print Queue ---"
        let input = File.ReadAllLines file

        let rules =
            input
            |> Seq.where (fun f -> f.Split("|").Length = 2)
            |> Seq.map (fun f ->
                f.Split("|")
                |> (fun f ->
                    { Lower = (f[0] |> Int32.Parse)
                      Upper = (f[1] |> Int32.Parse) }))
            |> Seq.toArray

        let updates =
            input
            |> Seq.where (fun f -> f.Split(",").Length > 1)
            |> Seq.map (fun f -> f.Split(",") |> Seq.map (fun f -> f |> Int32.Parse) |> Seq.toArray)

        let oneStar =
            updates
            |> Seq.map (fun f -> (InputMatchesRules f rules), f)
            |> Seq.filter (fun (result, _) -> result = true)
            |> Seq.map (fun (_, r) -> r[((r.Length / 2) |> int)])
            |> Seq.sum

        printfn $"⭐\tResult:\t%A{oneStar}"
        // printfn $"⭐⭐\tResult:\t%A{resultTwo ()}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
