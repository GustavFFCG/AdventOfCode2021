open System
open System.IO
open Microsoft.FSharp.Collections

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    strings
    |> Seq.map (fun s -> 
        s |> Seq.map (string >> Int32.Parse)
    ) |> array2D
    

let adjacentCoordinates x y arr =
    [x, y + 1; x + 1, y]
    |> List.filter (fun (x', y') -> 
        x' >= 0 && y' >= 0 && x' < Array2D.length1 arr && y' < Array2D.length2 arr)

let rec findPaths (x,y) path (arr: int[,]) =
    let pathHere = (x,y) :: path
    if x = Array2D.length1 arr - 1 && y = Array2D.length2 arr - 1
    then
        [pathHere]
    else
        adjacentCoordinates x y arr
        |> List.map (fun (nextx, nexty) -> findPaths (nextx, nexty) pathHere arr) 
        |> List.concat

let riskLevel (arr: int[,]) path =
    path
    |> List.fold (fun state (x,y) -> state + arr.[x,y]) -arr.[0,0]


let caveMap =
    File.ReadLines fileName
    |> toInput
caveMap
|> findPaths (0,0) [] 
|> List.map (fun p -> riskLevel caveMap p)
|> List.min
|> printfn "Min risk %i"