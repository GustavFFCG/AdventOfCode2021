open System

let mostCommonAtPosition (position: int) (data: string list) =
    data
    |> List.fold ( fun state s ->
        match s.[position] with
            | '1' -> state + 1
            | '0' -> state - 1
            | _ -> failwith "neither 0 or 1" )
        0
    |> fun x -> if x >= 0 then '1' else '0'

let rec oxygenGeneratorRating position (data: string list) =
    let filterValue = mostCommonAtPosition position data
    data
    |> List.filter (fun s -> s.[position] = filterValue )
    |> function
        | [endResult] -> endResult
        | [] -> failwith "We have an empty list. This should not happen"
        | filteredList -> filteredList |> oxygenGeneratorRating (position + 1) 

let rec co2ScrubberRating position (data: string list) =
    let filterValue = mostCommonAtPosition position data
    data
    |> List.filter (fun s -> s.[position] <> filterValue )
    |> function
        | [endResult] -> endResult
        | [] -> failwith "We have an empty list. This should not happen"
        | filteredList -> filteredList |> co2ScrubberRating (position + 1) 

let binaryAsDecimal s =
    Convert.ToInt32(s, 2)

let input = 
    fsi.CommandLineArgs |> Array.tail
    |> function 
        | [|filePath|] -> 
            System.IO.File.ReadLines(filePath)
            |> List.ofSeq
        | other -> failwith "Please provide input file"

let oxygen = input |> oxygenGeneratorRating 0

let co2 = input |> co2ScrubberRating 0

printfn "Oxygen generator is %s" oxygen
printfn "CO2 scrubber is %s" co2

(binaryAsDecimal oxygen) * (binaryAsDecimal co2)
|> printfn "Answer is: %i"  
