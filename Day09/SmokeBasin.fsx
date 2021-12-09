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
        s |> Seq.map (fun c -> Int32.Parse(string c))
    ) |> array2D
    
let data = 
    File.ReadLines(fileName)
    |> toInput

let adjacent x y arr =
    [x-1, y; x+1, y; x, y-1; x,y+1]
    |> List.filter (fun (x', y') -> 
        x' >= 0 && y' >= 0 && x' < Array2D.length1 arr && y' < Array2D.length2 arr)
    |> List.map (fun (x', y') -> arr.[x',y'])

let findLowPoints (arr: int[,]) =
    let maxX = Array2D.length1 arr - 1
    let maxY = Array2D.length2 arr - 1
    seq{0..maxX}
    |> Seq.fold ( fun colState x ->
        seq{0..maxY}
        |> Seq.fold ( fun rowState y ->
            let adjacentValues = adjacent x y arr
            if arr.[x,y] < List.min adjacentValues  then
                arr.[x,y] :: rowState
            else
                rowState
            )
            colState
        )
        []

data
|> findLowPoints
|> List.sumBy (fun i -> i+1)
|> printfn "Result: %A"