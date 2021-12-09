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

let adjacentPoints x y arr =
    [x-1, y; x+1, y; x, y-1; x,y+1]
    |> List.filter (fun (x', y') -> 
        x' >= 0 && y' >= 0 && x' < Array2D.length1 arr && y' < Array2D.length2 arr)

let findLowPoints arr =
    seq{0..Array2D.length1 arr - 1}
    |> Seq.fold ( fun colState x ->
        seq{0..Array2D.length2 arr - 1}
        |> Seq.fold ( fun rowState y ->
            let adjacentValues = adjacentPoints x y arr |> List.map (fun (x', y') -> arr.[x',y'])
            if arr.[x,y] < List.min adjacentValues  then
                (x,y) :: rowState
            else
                rowState
            )
            colState
        )
        []

let findBasins (arr: int [,]) =
    let rec expandBasin (basin: (int*int) Set) =
        let candidatePoints =
            basin
            |> Set.fold (fun state (x, y) -> 
                adjacentPoints x y arr
                |> List.filter (fun coord -> not (Set.contains coord basin)) 
                |> Set.ofList
                |> Set.union state
            ) Set.empty
        let pointIsInBasin x y =
            arr.[x,y] <> 9
            // adjacentPoints x y arr
            // |> List.filter (fun coord -> not (Set.contains coord basin))
            // |> List.exists (fun (x', y') -> (arr.[x',y'] > arr.[x,y]))

        let borderPoints = 
            candidatePoints
            |> Set.filter (fun (x,y) -> pointIsInBasin x y)
        if borderPoints = Set.empty then
            basin
        else
            borderPoints
            |> Set.fold ( fun state point -> state |> Set.add point ) basin
            |> expandBasin

    arr
    |> findLowPoints
    |> List.map (fun point -> [point] |> Set.ofList)
    |> List.map expandBasin


data
|> findLowPoints
|> List.sumBy (fun (x,y) -> data.[x,y]+1)
|> printfn "Sum of low points: %i"


let totalPoints = 
    data |> Seq.cast<int> |> Seq.length
let ninePoints = 
    data |> Seq.cast<int> |> Seq.filter (fun i -> i = 9) |> Seq.length
let pointsInBasins = 
    data |> findBasins |> List.sumBy Set.count 

printfn "Total points: %i Nines %i BasinPoints %i Unaccounted %i" totalPoints ninePoints pointsInBasins (totalPoints - ninePoints - pointsInBasins)
let basinPoints = data |> findBasins |> Seq.ofList |> Set.unionMany
let floormap = 
    data
    |> Array2D.map (string >> (fun s -> s.[0]))
    |> Array2D.mapi (fun x y c -> if Set.contains (x,y) basinPoints then ' ' else c)
let maxY = (Array2D.length1 floormap) - 1 
seq{0..maxY}
|> Seq.map ((fun i -> floormap.[i,*]) >> String)
|> Seq.iter (printfn "%s")


data
|> findBasins
|> List.map Set.count
|> List.sortDescending
|> List.take 3
|> fun x -> printfn "%A" x ; x
|> List.fold (fun state i -> state * i) 1
|> printfn "Product of top three basin sizes: %i"