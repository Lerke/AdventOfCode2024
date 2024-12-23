open System
open System.IO

type Node = { X: int; Y: int; Value: int }
type Edge = (int * int) * (int * int)

let Trailheads nodes (edges: Edge list) =
    nodes |> Seq.filter (fun f -> f.Value = 0)
    
let rec Traverse (trailhead: Node) (nodes: Node list) (edges: Edge list) =
    match (edges |> List.filter (fun ((x1,y1), (x2, y2)) -> x1 = trailhead.X && y1 = trailhead.Y)) with
    | [] -> failwithf ""
    | xs ->
        let n = nodes |> List.filter (fun f -> (xs |> List.tryFind (fun (_, (px, py)) -> px = f.X && py = f.Y)).IsSome)
        n |> List.map Traverse
    

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 10: Hoof It ---"
        let input = File.ReadAllLines file

        let arrayInput =
            input
            |> Seq.map (fun f -> f |> Seq.map (fun f -> Int32.Parse(f.ToString())) |> Seq.indexed |> Seq.toArray)
            |> Seq.indexed
            |> Seq.collect (fun (y, z) -> (z |> Array.map (fun (x, v) -> (x, y, v))))
            |> Seq.toArray


        let nodes = arrayInput |> Array.map (fun (x, y, v) -> { X = x; Y = y; Value = v })

        let edges =
            arrayInput
            |> Array.map (fun (x, y, z) ->
                (x,
                 y,
                 (seq {
                     (x - 1, y)
                     (x + 1, y)
                     (x, y - 1)
                     (x, y + 1)
                  }
                  |> Seq.map (fun (xx, yy) ->
                      (arrayInput
                       |> Array.tryFind (fun (px, py, pv) -> xx = px && yy = py && (z + 1) = pv)))
                  |> Seq.filter _.IsSome
                  |> Seq.map _.Value
                  |> Seq.toArray)))
            |> Array.collect (fun (x,y,v) -> v |> Array.map(fun (xx, yy, _) -> (x,y,xx,yy)))

        let oneStar = 1
        let twoStar = 2
        printfn $"⭐\tResult:\t%A{arrayInput}"
        printfn $"⭐⭐\tResult:\t%A{edges}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
