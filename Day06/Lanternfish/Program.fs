open System

module Lanternfish =
    let toHistogram (rawList: int list) =
        rawList
        |> List.fold (fun state i -> 
            Map.tryFind i state
            |> function
                | None -> state |> Map.add i 1L
                | Some previousCount -> state |> Map.add i (1L + previousCount)
            ) Map.empty

    let count (h: Map<int,int64>) =
        Map.fold (fun state _key count -> state + count) 0L h

    let regenerate (h: Map<int,int64>) =
        Map.fold 
            (fun state key count -> 
                match key with
                    | 0 ->
                        state
                        |> Map.add 8 count
                        |> Map.change 6 (function | Some c -> Some (c + count) | None -> Some count)
                    | k -> 
                        state 
                        |> Map.change ( k - 1 ) (function | Some c -> Some (c + count) | None -> Some count) 
            )
            Map.empty h

    let regenerateN n (h: Map<int,int64>) =
        seq {1..n}
        |> Seq.fold (fun state _i -> regenerate state) h
        

[<EntryPoint>]
let main argv =
    let exampleInput = [3;4;3;1;2]
    let assignmentInput = 
        [3;5;3;1;4;4;5;5;2;1;4;3;5;1;3;5;3;2;4;3;5;3;1;1;2;1;4;5;3;1;4;5;4;3;3;4;3;1;1;2;2;4;1;1;4;3;4;4;2;4;3;1;5;1;2;3;2;4;4;1;1;1;3;3;5;1;4;5;5;2;5;3;3;1;1
         2;3;3;3;1;4;1;5;1;5;3;3;1;5;3;4;3;1;4;1;1;1;2;1;2;3;2;2;4;3;5;5;4;5;3;1;4;4;2;4;4;5;1;5;3;3;5;5;4;4;1;3;2;3;1;2;4;5;3;3;5;4;1;1;5;2;5;1;5;5;4;1;1;1;1
         5;3;3;4;4;2;2;1;5;1;1;1;4;4;2;2;2;2;2;5;5;2;4;4;4;1;2;5;4;5;2;5;4;3;1;1;5;4;5;3;2;3;4;1;4;1;1;3;5;1;2;5;1;1;1;5;1;1;4;2;3;4;1;3;3;2;3;1;1;4;4;3;2;1;2
         1;4;2;5;4;2;5;3;2;3;3;4;1;3;5;5;1;3;4;5;1;1;3;1;2;1;1;1;1;5;1;1;2;1;4;5;2;1;5;4;2;2;5;5;1;5;1;2;1;5;2;4;3;2;3;1;1;1;2;3;1;4;3;1;2;3;2;1;3;3;2;1;2;5;2]

    exampleInput
    |> Lanternfish.toHistogram
    |> Lanternfish.regenerateN 80
    |> Lanternfish.count
    |> printfn "There are %i lanternfish in example input after 80 days."

    assignmentInput
    |> Lanternfish.toHistogram
    |> Lanternfish.regenerateN 80
    |> Lanternfish.count
    |> printfn "There are %i lanternfish in assignment input after 80 days."

    exampleInput
    |> Lanternfish.toHistogram
    |> Lanternfish.regenerateN 256
    |> Lanternfish.count
    |> printfn "There are %i lanternfish in example input after 256 days."

    assignmentInput
    |> Lanternfish.toHistogram
    |> Lanternfish.regenerateN 256
    |> Lanternfish.count
    |> printfn "There are %i lanternfish in assignment input after 256 days."
    0 // return an integer exit code
