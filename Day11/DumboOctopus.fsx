open System
open System.IO
open Microsoft.FSharp.Collections

type Octopus = 
    | Charging of int
    | AboutToFlash
    | Flashed
module Octopus =
    let ofInt i = 
        if i >= 9 then AboutToFlash else Charging i
    let increaseEnergy = function
        | Charging i when i >= 9 -> AboutToFlash
        | Charging i -> Charging (i + 1)
        | AboutToFlash -> AboutToFlash
        | Flashed -> Flashed
    let tryFlash = function
        | Charging i -> false, Charging i
        | AboutToFlash -> true, Flashed
        | Flashed -> false, Flashed
    let reset = function
        | Charging i -> Charging i
        | AboutToFlash -> failwith "cannot reset before flash"
        | Flashed -> Charging 0

type OctoPool = {
    Octopuses: Octopus[,]
    FlashCount: int
}

let adjacentCoordinates x y arr =
    [
        x-1, y-1; x-1, y; x-1, y+1
        x, y-1; x,y+1
        x+1, y-1; x+1, y; x+1, y+1 
    ]
    |> List.filter (fun (x', y') -> 
        x' >= 0 && y' >= 0 && x' < Array2D.length1 arr && y' < Array2D.length2 arr)
    |> Set.ofList

let tryFlashAt x y (pool: OctoPool) = 
    let (flashed, octopusAtCoord) = pool.Octopuses.[x,y] |> Octopus.tryFlash
    {
        Octopuses =  
            pool.Octopuses
            |> Array2D.mapi (fun x' y' o ->
                if (x',y') = (x,y) then octopusAtCoord
                else 
                    if Set.contains (x',y') (adjacentCoordinates x y pool.Octopuses) && flashed then
                        o |> Octopus.increaseEnergy
                    else
                        o)
        FlashCount = if flashed then pool.FlashCount + 1 else pool.FlashCount
    }


let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    strings
    |> Seq.map (fun s -> 
        s |> Seq.map (string >> Int32.Parse >> Octopus.ofInt)
    ) |> array2D

let startingPool =  {
    Octopuses = 
        File.ReadLines(fileName)
        |> toInput
    FlashCount = 0
}

let rec flash octopool =
    let newState = 
        seq{
            for x in 0..(Array2D.length1 octopool.Octopuses - 1) do
                for y in 0..(Array2D.length2 octopool.Octopuses - 1) ->
                    (x, y)
        }
        |> Seq.fold (fun state (x, y) -> tryFlashAt x y state) octopool
    if newState.Octopuses |> Seq.cast |> Seq.exists (fun o -> o = AboutToFlash) then
        flash newState
    else
        newState

let step octopool =
    { octopool with Octopuses = octopool.Octopuses |> Array2D.map Octopus.increaseEnergy }
    |> flash
    |> fun pool -> {pool with Octopuses = pool.Octopuses |> Array2D.map Octopus.reset }

let numberOfSteps = 100
seq{1..numberOfSteps}
|> Seq.fold (fun state i -> 
        step state
    ) startingPool
|> fun o -> printfn "Total %i flashes after %i steps" o.FlashCount numberOfSteps

let rec stepUntilAllHaveFlashed i state =
    if i > 500 then failwith "gave up"
    if not (state.Octopuses |> Seq.cast |> (Seq.exists (fun o -> o <> Charging 0)) )
    then i
    else step state |> stepUntilAllHaveFlashed (i + 1)

stepUntilAllHaveFlashed 0 startingPool
|> printfn "All octopuses flashed at step %i"