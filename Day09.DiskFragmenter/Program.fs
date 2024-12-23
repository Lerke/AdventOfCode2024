open System
open System.IO
open Microsoft.FSharp.Core

type BlockEntry = { Id: int }

type DiskItem =
    | Entry of BlockEntry
    | Free

type BlockRange =
    { First: BlockEntry
      Index: int
      Length: int }

let ParseInput (line: string) =
    seq {
        let mutable id = 0

        for x in (line.ToCharArray() |> Seq.chunkBySize 2) do
            match x.Length with
            | 0 -> yield! seq { }
            | 1 ->
                yield!
                    (seq { 0 .. (x[0].ToString() |> Int32.Parse) - 1 })
                    |> Seq.map (fun _ -> (Entry { Id = id }))
            | 2 ->
                yield!
                    (seq { 0 .. (x[0].ToString() |> Int32.Parse) - 1 })
                    |> Seq.map (fun _ -> (Entry { Id = id }))

                id <- id + 1

                yield! (seq { 0 .. (x[1].ToString() |> Int32.Parse) - 1 }) |> Seq.map (fun _ -> Free)
            | _ -> failwith "Unsupported chunk"
    }

let BlockMap (input: DiskItem array) =
    input
    |> Array.indexed
    |> Array.filter (fun f -> (f |> snd).IsEntry)
    |> Array.map (fun f ->
        match (f |> snd) with
        | Entry e -> (f |> fst, e)
        | _ -> failwith "Unsupported grouping")
    |> Array.groupBy (fun f -> (f |> snd).Id)
    |> Array.map (fun (idx, f) ->
        { First = (f[0] |> snd)
          Index = (f |> Seq.map fst |> Seq.min)
          Length = f.Length })

let Compact (array: DiskItem array) =
    let rec _Compact array startIdx endIdx =
        match (startIdx >= endIdx) with
        | true -> array
        | false ->
            match (Array.tryItem startIdx array), (Array.tryItem endIdx array) with
            | Some lhs, Some rhs ->
                match (lhs, rhs) with
                | Free, Free -> _Compact array startIdx (endIdx - 1)
                | Free, Entry e ->
                    array[startIdx] <- Entry e
                    array[endIdx] <- Free
                    _Compact array (startIdx + 1) (endIdx - 1)
                | Entry _, Free -> _Compact array startIdx (endIdx - 1)
                | Entry _, Entry _ -> _Compact array (startIdx + 1) endIdx
            | _ -> failwithf ""

    _Compact (array |> Array.map id) 0 (array.Length - 1)

let rec CompactBlocks (array: DiskItem array) (blocks: BlockRange array) =
    let b =
        blocks
        |> Seq.rev
        |> Seq.map (fun bb ->
            let fIndex =
                array
                |> Array.indexed
                |> Array.tryFindIndex (fun (i, f) ->
                    f.IsFree
                    && i < bb.Index
                    && (seq { i .. (i + bb.Length - 1) }
                        |> Seq.forall (fun b -> array.Length > b && array[b].IsFree)))

            match fIndex with
            | Some _ -> (true, Some fIndex, Some bb)
            | None -> (false, None, None))
        |> Seq.tryFind (fun (r, _, _) -> r)

    match b with
    | Some(_, fIndex, bb) ->
        let mutable ix = 0

        for i in seq { fIndex.Value.Value .. (fIndex.Value.Value + (bb.Value.Length - 1)) } do
            array[i] <- array[bb.Value.Index + ix]
            array[bb.Value.Index + ix] <- Free
            ix <- ix + 1

        CompactBlocks array (blocks |> (Array.take (Array.IndexOf(blocks, bb.Value))))
    | None -> array

match Environment.GetCommandLineArgs() with
| [| _; file |] ->
    match File.Exists(file) with
    | true ->
        printfn "--- Day 09: Disk Fragmenter ---"
        let input = File.ReadAllText file
        let parsed = ParseInput input |> Seq.toArray
        let compacted = parsed |> Compact

        let oneStar =
            compacted
            |> Array.indexed
            |> Array.fold
                (fun acc (idx, curr) ->
                    acc
                    + (match curr with
                       | Entry entry -> (entry.Id |> int64) * (idx |> int64)
                       | Free -> 0L))
                0L

        let blockMap = BlockMap parsed
        let parsed = ParseInput input |> Seq.toArray

        let twoStar =
            CompactBlocks parsed blockMap
            |> Array.indexed
            |> Array.fold
                (fun acc (idx, curr) ->
                    acc
                    + (match curr with
                       | Entry entry -> (entry.Id |> int64) * (idx |> int64)
                       | Free -> 0L))
                0L

        printfn $"⭐\tResult:\t%A{oneStar}"
        printfn $"⭐⭐\tResult:\t%A{twoStar}"
        0
    | false -> failwithf "File not found"
| _ -> failwithf "Usage: ./dotnet run <path-to-puzzle-input>"
|> ignore
