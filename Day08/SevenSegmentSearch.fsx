open System
open System.IO
open FSharp.Core

type input = {
    Samples: char Set list
    Readings: char Set list
}

module Segments =
    let segments = Set.ofList ['a';'b';'c';'d';'e';'f';'g'] 
    let ofString = Set.ofSeq
    let toObviousInt (s: char Set) =
        match s.Count with
            | 2 -> Some 1
            | 4 -> Some 4
            | 3 -> Some 7
            | 7 -> Some 8
            | _ -> None
    let toInt (samples: char Set list) (s: char Set) =
        //  aa 
        // b  c
        // b  c
        //  dd
        // e  f
        // e  f
        //  gg

        let one =
            samples
            |> List.find (fun s -> s.Count = 2) //this is 1
        let four =
            samples
            |> List.find (fun s -> s.Count = 4) //this is 1
        let seven =
            samples
            |> List.find (fun s -> s.Count = 3) //this is 7
        let eight = segments
        let fourWithoutOne = Set.difference four one

        match s.Count with
            | 2 -> Some 1
            | 4 -> Some 4
            | 3 -> Some 7
            | 7 -> Some 8
            | 5 -> //this is 2, 3 or 5
                if Set.isSubset one s then Some 3
                else if Set.isSubset fourWithoutOne s then Some 5
                else Some 2
            | 6 -> //this is 0, 6 or 9
                if not (Set.isSubset one s) then Some 6
                else if Set.isSubset four s then Some 9
                else Some 0
            | _ -> None


let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (s: string) =
    s.Split([| " | "|], StringSplitOptions.None)
    |> fun arr -> 
        let toSegmentsList (s: string) = 
            s.Split([|' '|]) 
            |> List.ofArray
            |> List.map Segments.ofString
        {
            Samples = arr.[0] |> toSegmentsList 
            Readings = arr.[1] |> toSegmentsList 
        }

let readingToInt reading =
    reading
    |> List.rev
    |> List.mapi (fun i x -> (pown 10 i) * x)
    |> List.sum

let data = 
    File.ReadLines(fileName)
    |> Seq.map toInput

data
|> Seq.sumBy (fun input ->
    input.Readings
    |> List.sumBy  (Segments.toObviousInt >> function | Some i -> 1 | None -> 0)
)
|> printfn "Input has %i instances of 1, 4, 7 and 8"

data
|> Seq.sumBy (fun input ->
    input.Readings
    |> List.map (fun r -> Segments.toInt input.Samples r |> function | Some i -> i | None -> failwithf "Could not interpret %A" input)
    |> readingToInt
)
|> printfn "Sum of all samples is %i"
