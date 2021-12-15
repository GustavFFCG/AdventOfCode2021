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
    
let findPaths start target =
    if start.x = target.x && start.y + 2 = target.y then
        Some [ [Coord.ofxy start.x (start.y + 1); Coord.ofxy start.x (start.y + 2)] ]
    else if start.x + 2 = target.x && start.y = target.y then
        Some [ [Coord.ofxy (start.x + 1)  start.y; Coord.ofxy (start.x + 2) start.y] ]
    else if start.x + 1 = target.x && start.y + 1 = target.y then
        Some [ [Coord.ofxy (start.x + 1) start.y; Coord.ofxy (start.x + 1) (start.y + 1)]; [ Coord.ofxy start.x (start.y + 1); Coord.ofxy (start.x + 1) (start.y + 1)] ]
    else None

let riskLevel (arr: int[,]) (path:Coord list) =
    path
    |> List.sumBy (fun coord -> arr.[coord.x,coord.y])

let diagonal length i  =
    seq{0..length - 1} |> List.ofSeq
    |> List.map (fun x -> (x, 2*i - x ) )
    |> List.filter (fun (x, y) -> x >= 0 && y >= 0 && x < length && y < length)

module CaveMap =
    let array =
        File.ReadLines fileName
        |> toInput
    let size = Array2D.length1 array


let riskToDiagonal i (riskToPrevDiagonal:CoordinateRisk list): CoordinateRisk list =
    let risk coord =
        riskToPrevDiagonal
        |> List.map (fun risk ->
            findPaths risk.Coord coord
            |> Option.map (List.map (fun path -> 
                risk.Risk + riskLevel CaveMap.array path))
            |> Option.map List.min
        )
        |> List.choose id 
        |> List.min

    diagonal CaveMap.size i
    |> List.map (fun (x,y) -> { Coord={x=x; y=y}; Risk = risk (Coord.ofxy x y) } )

let diagonalZero = [ {Coord={x=0; y=0}; Risk = 0 } ]

seq{1..CaveMap.size - 1}
|> Seq.fold (fun state i -> riskToDiagonal i state) diagonalZero
|> Seq.head
|> fun risk -> printfn "Risk is %i" risk.Risk
