open System
open System.IO
open Microsoft.FSharp.Collections

type Coord = {x: int; y:int}
    with static member ofxy x y = {x=x; y=y}

type AggregateRisk =
| Unknown
| Preliminary of int
| Final of int
module AggregateRisk =
    let isFinal = function | Final _ -> true |_ -> false
    let finalRisk = function | Final r -> Some r | _ -> None

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput strings =
    strings
    |> Seq.map (fun s -> 
        s |> Seq.map (string >> Int32.Parse)) 
    |> array2D

    // strings
    // |> Seq.mapi (fun y s -> 
    //     s |> Seq.mapi (fun x c -> (Coord.ofxy x y), c |> string |> Int32.Parse |> (fun i -> i, if (x,y) = (0,0) then Preliminary 0 else Unknown))) 
    // |> Seq.concat
    // |> Map.ofSeq
    // |> fun m -> (strings |> Seq.length), m

module CaveMap =
    let arr =
        File.ReadLines fileName
        |> toInput
    let size = arr |> Array2D.length1
    
    let adjacentCoordinates coord =
        [
            coord.x-1, coord.y
            coord.x, coord.y-1; coord.x,coord.y+1
            coord.x+1, coord.y
        ]
        |> List.map (fun (x,y) -> Coord.ofxy x y)
        |> List.filter (fun c -> 
            c.x >= 0 && c.y >= 0 && c.x < size && c.y < size)
    let localRisk coord = arr.[coord.x, coord.y]
    let aggregateRisk coord m = m |> Map.tryFind coord |> Option.defaultValue Unknown
    
    let isComplete (m: Map<Coord,AggregateRisk>) = 
        not(m |> Map.exists (fun c risk -> not (AggregateRisk.isFinal risk) ))
    

let rec dijkstra  (cave: Map<Coord,AggregateRisk>) =
    if CaveMap.isComplete cave then cave
    else
        let minFinalNode = 
            cave
            |> Map.toSeq
            |> Seq.map (fun (c, risk) -> 
                match risk with
                | Preliminary r -> Some (c,r) 
                | _ -> None)
            |> Seq.choose id
            |> Seq.minBy (fun (_c, i) -> i)
        
        let newCave = 
            CaveMap.adjacentCoordinates (fst minFinalNode)
            |> List.filter (fun c -> 
                cave |> CaveMap.aggregateRisk c |> AggregateRisk.isFinal |> not
            )
            |> List.fold (fun state c ->
                let aggregate = CaveMap.aggregateRisk c cave
                let risk = CaveMap.localRisk c
                let newAggregate = risk + (snd minFinalNode)
                match aggregate with
                    | Unknown -> 
                        state |> Map.add c (Preliminary newAggregate)
                    | Preliminary r when r > newAggregate ->
                        state |> Map.add c (Preliminary newAggregate)
                    | _ -> state
                ) cave
            |> Map.add (fst minFinalNode) (Final (snd minFinalNode))
        dijkstra newCave


let startingMap = Map.empty |> Map.add (Coord.ofxy 0 0) (Preliminary 0)
dijkstra startingMap
|> Map.find (Coord.ofxy (CaveMap.size - 1) (CaveMap.size - 1))
|> printfn "Risk is %A"
