open System
open System.Text.RegularExpressions

type Coordinate = {X: int; Y: int}
type Line = {Start: Coordinate; Stop: Coordinate}

module Line =
    let isStraight (l: Line) =
        l.Start.X = l.Stop.X || l.Start.Y = l.Stop.Y

    let points (l: Line) =
        if l.Start.X = l.Stop.X then
            seq {min l.Start.Y l.Stop.Y .. max l.Start.Y l.Stop.Y}
            |> Seq.map (fun y -> {X = l.Start.X; Y = y})
        else
            if l.Start.Y = l.Stop.Y then
                seq {min l.Start.X l.Stop.X .. max l.Start.X l.Stop.X}
                |> Seq.map (fun x -> {X = x; Y = l.Start.Y})
            else
                if l.Start.X <= l.Stop.X then
                    l
                else //reverse line for simplicity
                    {Start = l.Stop; Stop = l.Start}
                |> fun leftToRightLine ->
                    seq {leftToRightLine.Start.X .. leftToRightLine.Stop.X}
                    |> Seq.map (fun x -> 
                        if leftToRightLine.Start.Y <= leftToRightLine.Stop.Y then
                            {X = x; Y = leftToRightLine.Start.Y + (x - leftToRightLine.Start.X) }
                        else
                            {X = x; Y = leftToRightLine.Start.Y - (x - leftToRightLine.Start.X) }
                        )


module Input =
    let parseInputRow r =
            let regex = "(\d*),(\d*) -> (\d*),(\d*)"
            let m = Regex(regex).Match(r)
            if m.Success
            then
                let intFromGroup (group: int) =
                    int m.Groups.[group].Value
                { Start = { X = intFromGroup 1 ; Y = intFromGroup 2 } ; Stop = { X = intFromGroup 3 ; Y = intFromGroup 4 } }
            else failwith $"Unexpected input: {r}"

    let input filePath =
        try
            System.IO.File.ReadLines(filePath)
            |> Seq.map parseInputRow
            |> Ok
        with ex ->
            sprintf "Exception %s" ex.Message |> Error

let removeDiagonals l = Seq.filter Line.isStraight l

let plotDangerPoints (oldPlot: Map<Coordinate, int>) line =
    Line.points line
    |> Seq.fold (fun state c ->
        Map.tryFind c state
        |> function
            | None -> state |> Map.add c 1
            | Some i -> state |> Map.add c (i + 1)
    ) oldPlot

let plotDangerLines (oldPlot: Map<Coordinate, int>) (lines: Line seq) =
    Seq.fold (fun state line -> plotDangerPoints state line) oldPlot lines

let printDangerPoints (map: Map<Coordinate, int>) =
    map
    |> Map.filter (fun c i -> i > 1)
    |> Map.count

let solveForStraightLines lines =
    let dangerPoints = Map.empty
    lines
    |> removeDiagonals
    |> plotDangerLines dangerPoints
    |> printDangerPoints
    |> printfn "Number of danger points with straight lines only: %i"

let solveForAllLines lines =
    let dangerPoints = Map.empty
    lines
    |> plotDangerLines dangerPoints
    |> printDangerPoints
    |> printfn "Number of danger points with all lines: %i"

[<EntryPoint>]
let main argv =
    argv
    |> List.ofArray
    |> function
        | [] -> printfn "Please provide input file"
        | file :: _tail ->
            Input.input file
            |> Result.map (fun lines ->
                solveForStraightLines lines   
                solveForAllLines lines   
            )
            |> function 
                | Ok () -> ()
                | Error e -> printfn "Some error occurred %s" e

    0 // return an integer exit code
