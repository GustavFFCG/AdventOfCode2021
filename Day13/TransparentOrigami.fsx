open System
open System.IO
open Microsoft.FSharp.Collections

type Fold = 
    | XFold of int
    | YFold of int
module Fold =
    let fromString (s: string) =
        s.Split [|'='|]
        |> fun arr -> arr.[0], int arr.[1]
        |> function
            | "x", i -> XFold i
            | "y", i -> YFold i
            | _ -> failwith "unexpected fold"
    let transformCoord fold (x, y) =
        match fold with
            | XFold l ->
                if x = l then failwith $"Point {x}, {y} in x fold {l}" 
                else 
                    if x < l then (x, y)
                    else (2 * l - x, y)
            | YFold l ->
                if y = l then failwith $"Point {x}, {y} in y fold {l}" 
                else 
                    if y < l then (x, y)
                    else (x, 2 * l - y)

type Indata = {
    Points: (int*int) Set
    Folds: Fold list
}

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    { 
        Points =
            strings
            |> Seq.filter (fun s -> s.Contains(",") )
            |> Seq.map (fun s -> s.Split [|','|] |> (fun arr -> int arr.[0], int arr.[1] ))
            |> Set.ofSeq
        Folds =
            let removeStart (s: string) = s.Substring(11)
            strings
            |> Seq.filter (fun s -> s.StartsWith("fold along") )
            |> Seq.map (removeStart >> Fold.fromString)
            |> List.ofSeq
    }

let sheetToString (sheet: (int*int) Set) =
    let maxX = sheet |> Set.map (fun (x, _y) -> x) |> Set.maxElement
    let maxY = sheet |> Set.map (fun (_x, y) -> y) |> Set.maxElement
    seq {0..maxY}
    |> Seq.map ( fun y -> 
        let points = 
            sheet
            |> Set.filter (fun (_, y') -> y' = y)
            |> Set.map (fun (x, y) -> x)
        Array.create (maxX + 1) '.'
        |> Array.mapi (fun i c -> if Set.contains i points then '#' else '.')
    >> String >> fun s -> s + Environment.NewLine
    ) 
    |>Seq.fold (fun state s -> state + s) ""

let data = 
    File.ReadLines(fileName)
    |> toInput

// first assignment
let firstFold = data.Folds |> List.head

data.Points
|> Set.map (Fold.transformCoord firstFold)
|> Set.count
|> printfn "We have %i points"

//Second
data.Folds
|> List.fold (fun points f -> points |> Set.map (Fold.transformCoord f)) data.Points
|> sheetToString
|> printfn "%s"

