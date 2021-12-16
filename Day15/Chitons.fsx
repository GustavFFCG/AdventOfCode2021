open System
open System.IO
open Microsoft.FSharp.Collections

type Coord = {x: int; y:int}
    with static member ofxy x y = {x=x; y=y}

type CoordinateRisk = {
    Coord: Coord
    Risk: int
}

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    strings
    |> Seq.map (fun s -> 
        s |> Seq.map (string >> Int32.Parse)
    ) |> array2D

module CaveMap =
    let array =
        File.ReadLines fileName
        |> toInput
    let size = Array2D.length1 array
    let adjacentCoordinates coord =
        [
            coord.x-1, coord.y
            coord.x, coord.y-1; coord.x,coord.y+1
            coord.x+1, coord.y
        ]
        |> List.map (fun (x,y) -> Coord.ofxy x y)
        |> List.filter (fun c -> 
            c.x >= 0 && c.y >= 0 && c.x < size && c.y < size)

let rec findPaths path coord target =
    let pathHere = coord :: path
    if coord = target then
        Some [pathHere]
    else
        let candidatePoints = CaveMap.adjacentCoordinates coord
        if List.contains target candidatePoints then 
            Some [target :: pathHere]
        else
            let pointIsInPath c =
                List.contains c path
            let pointIsAdjacentToPath c =
                CaveMap.adjacentCoordinates c
                |> List.exists (fun c' -> List.contains c' path)
            candidatePoints
            |> List.filter (fun c -> not(pointIsInPath c || pointIsAdjacentToPath c))
            |> function
                | [] -> None
                | l -> 
                    l
                    |> List.map (fun c -> findPaths pathHere c target)
                    |> List.choose id
                    |> List.concat
                    |> Some

let riskLevel (arr: int[,]) (path:Coord list) =
    path
    |> List.sumBy (fun coord -> arr.[coord.x,coord.y])

let diagonal length i  =
    seq{0..length - 1} |> List.ofSeq
    |> List.map (fun x -> {x=x;y=2*i - x} )
    |> List.filter (fun c -> c.x >= 0 && c.y >= 0 && c.x < length && c.y < length)

let riskToDiagonal i (riskToPrevDiagonal:CoordinateRisk list): CoordinateRisk list =
    let risk coord =
        riskToPrevDiagonal
        |> List.map (fun risk ->
            findPaths [] risk.Coord coord
            |> Option.map (List.map (fun path -> 
                    risk.Risk + riskLevel CaveMap.array path))
            |> Option.map List.min
        )
        |> List.choose id
        |> List.min

    diagonal CaveMap.size i
    |> List.map (fun c -> { Coord=c; Risk = risk c } )

let diagonalZero = [ {Coord={x=0; y=0}; Risk = 0 } ]

seq{1..CaveMap.size - 1}
|> Seq.fold (fun state i -> riskToDiagonal i state) diagonalZero
|> Seq.head
|> fun risk -> printfn "Risk is %i" risk.Risk
