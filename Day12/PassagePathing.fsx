open System
open System.IO
open Microsoft.FSharp.Collections

type CaveType =
    | Start
    | End
    | SmallCave
    | LargeCave
type Cave = {
    Name: string
    Exits: string list
}

module Cave =
    let caveType = function
        | "start" -> Start
        | "end" -> End
        | s when s = s.ToUpper() -> LargeCave
        | s when s = s.ToLower() -> SmallCave
        | s -> failwith $"illegal cave name {s}"

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    strings
    |> Seq.map (fun s -> s.Split [|'-'|] |> (fun arr -> arr.[0], arr.[1] ))
    |> List.ofSeq
    
let caves = 
    File.ReadLines(fileName)
    |> toInput
    |> fun input ->
        let uniqueNames =
            input
            |> List.fold (fun state (s1, s2) -> state |> Set.add s1 |> Set.add s2) Set.empty
        let connections name =
            input
            |> List.fold (fun state (s1, s2) ->
                match s1, s2 with
                    | n, exit when n = name  -> state |> Set.add exit 
                    | exit, n when n = name -> state |> Set.add exit 
                    | _ -> state) Set.empty

        uniqueNames
        |> Set.map (fun name -> 
            {
                Name = name
                Exits = connections name |> List.ofSeq
            }
        )
        |> Seq.map (fun c -> c.Name, c)
        |> Map.ofSeq

let rec findPaths1 cave path (caves: Map<string, Cave>) =
    let pathHere = cave.Name :: path
    if Cave.caveType cave.Name = End 
    then
        [pathHere]
    else
        cave.Exits
        |> List.filter (fun (s: string) -> (Cave.caveType s = LargeCave) || not( pathHere |> List.exists (fun s' -> s'= s)))
        |> List.map (fun name -> 
            caves
            |> Map.find name
            |> fun c -> findPaths1 c pathHere caves)
        |> List.concat

let rec findPaths2 cave path (caves: Map<string, Cave>) =
    let pathHere = cave.Name :: path
    if Cave.caveType cave.Name = End 
    then
        [pathHere]
    else
        cave.Exits
        |> List.filter (fun s ->
                let aSmallCaveVisitedTwice =
                    pathHere
                    |> List.filter (fun s -> Cave.caveType s = SmallCave)
                    |> List.groupBy id
                    |> List.exists (fun (_key, l) -> List.length l > 1 )
                (Cave.caveType s = LargeCave)
                || (Cave.caveType s = SmallCave && not aSmallCaveVisitedTwice)
                || not ( pathHere |> List.exists (fun s' -> s'= s) )
                )
        |> List.map (fun name -> 
            caves
            |> Map.find name
            |> fun c -> findPaths2 c pathHere caves)
        |> List.concat

let entrance = caves |> Map.find "start"

caves
|> findPaths1 entrance []
|> List.length
|> printfn "Number of paths rules 1: %i"

caves
|> findPaths2 entrance []
|> List.length
|> printfn "Number of paths rules 2: %i"