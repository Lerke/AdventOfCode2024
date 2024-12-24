open System
open System.IO

type Node = { X: int; Y: int; Value: int }
type Edge = (int * int) * (int * int)

let Trailheads nodes =
    nodes |> Seq.filter (fun f -> f.Value = 0)

let Traverse (trailhead: Node) (nodes: Node list) (edges: Edge list) =
    let rec _traverse (trailhead: Node) (nodes: Node list) (edges: Edge list) (traversed: Node list) =
        match trailhead with
        | th when th.Value = 9 -> [ th ]
        | _ ->
            match
                (edges
                 |> List.filter (fun ((x1, y1), (_, _)) -> x1 = trailhead.X && y1 = trailhead.Y))
            with
            | [] -> []
            | xs ->
                let n =
                    nodes
                    |> List.filter (fun f -> (xs |> List.tryFind (fun (_, (px, py)) -> px = f.X && py = f.Y)).IsSome)

                n
                |> List.where (fun f -> (traversed |> List.contains f) = false)
                |> List.map (fun f -> _traverse f nodes edges (traversed @ [ f ]))
                |> List.collect id

    (match _traverse trailhead nodes edges [] with
     | x when (x |> List.last).Value = 9 -> x
     | _ -> [])
    |> List.distinctBy (fun f -> f.X, f.Y)


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
            |> Seq.toList


        let nodes = arrayInput |> List.map (fun (x, y, v) -> { X = x; Y = y; Value = v })

        let edges =
            arrayInput
            |> List.map (fun (x, y, z) ->
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
                       |> List.tryFind (fun (px, py, pv) -> xx = px && yy = py && (z + 1) = pv)))
                  |> Seq.filter _.IsSome
                  |> Seq.map _.Value
                  |> Seq.toList)))
            |> Seq.toList
            |> List.collect (fun (x, y, v) -> v |> List.map (fun (xx, yy, _) -> (x, y), (xx, yy)))


        let ths = Trailheads nodes
        let oneStar = ths |> Seq.map (fun f -> Traverse f nodes edges) |> Seq.sumBy _.Length
        let twoStar = 2
        printfn $"⭐\tResult:\t%A{oneStar}"
        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
