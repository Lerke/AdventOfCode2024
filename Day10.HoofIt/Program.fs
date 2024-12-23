open System
open System.IO

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 10: HooF It ---"
        let input = File.ReadAllLines file
        let arrayInput =
            input
            |> Seq.map (fun f -> f |> Seq.map(fun f -> Int32.Parse(f.ToString())) |> Seq.toArray)
            |> Seq.toArray
        
        
        let oneStar = 1
        let twoStar = 2
        printfn $"⭐\tResult:\t%A{arrayInput}"
        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
