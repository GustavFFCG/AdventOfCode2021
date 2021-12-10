open System
open FSharp.Collections

type NumberStatus =
    | Drawn
    | NotDrawn
    
type Board = (int * NumberStatus) [,]
module Board = 
    let rows (board: Board) : (int * NumberStatus) list list =
        [
            for row in 0 .. (Array2D.length1 board) - 1 do
            yield board.[row,*] |> List.ofArray
        ]

    let cols (board: Board) : (int * NumberStatus) list list =
        [
            for col in 0 .. (Array2D.length2 board) - 1 do
            yield board.[*,col] |> List.ofArray
        ]
        
    let markNumber number (board: Board) : Board =
        board 
        |> Array2D.map (fun (n, s) -> 
            if n = number then (n, Drawn)
            else (n, s)
        )
         
    let winning (board: Board) =
        board |> rows
        |> List.exists (fun row -> 
            row |> List.forall (fun (_n, status) -> status = Drawn)
        )
        |> function
            | true -> true
            | false -> 
                board |> cols
                |> List.exists (fun col -> 
                    col |> List.forall (fun (_n, status) -> status = Drawn)
                )

    let sumOfUnmarked (board: Board) =
        board 
        |> Seq.cast<int*NumberStatus>
        |> Seq.filter (fun (_n, status) -> status = NotDrawn)
        |> Seq.fold (fun i (n, _) -> i + n ) 0

type Input = {
    Numbers: int seq
    Boards: Board list
}

let numbersFromInput (numberInput: string) =
    numberInput.Split [|','|]
    |> Seq.map int
    |> List.ofSeq

let singleBoardFromInput (boardInput: string list) : Board =
    boardInput
    |> List.map (
        Seq.splitInto 5
        >> Seq.map ( String >> (fun x -> x.Replace(" ", "") ))
        >> Seq.map int
        >> Seq.map ( fun i -> ( i, NumberStatus.NotDrawn ) )
        )
    |> array2D

let boardsFromInput (boardInput: string list) =
    boardInput 
    |> List.filter (fun s -> not (String.IsNullOrWhiteSpace s))
    |> List.chunkBySize 5
    |> List.map singleBoardFromInput

let input filePath =
    try
        System.IO.File.ReadLines(filePath)
        |> List.ofSeq
        |> function
            | numberInput::boardInput ->
                (numbersFromInput numberInput, boardsFromInput boardInput)
                |> Ok
            | _ -> Error "Unexpected input"
    with ex ->
        sprintf "Exception %s" ex.Message |> Error

let rec findFirstWinner (numbers: int list) (boards: Board list)  =
    match numbers with 
     | [] -> Error "We ran out of numbers"
     | head :: tail ->
        let newBoards = boards |> List.map (Board.markNumber head)
        let maybeWinner = newBoards |> List.tryFind Board.winning
        match maybeWinner with
            | Some winner -> (Board.sumOfUnmarked winner) * head |> Ok
            | None -> findFirstWinner tail newBoards

let rec findLastWinner (numbers: int list) (boards: Board list)  =
    match numbers with 
     | [] -> Error "We ran out of numbers"
     | head :: tail ->
        let newBoards = boards |> List.map (Board.markNumber head)
        match newBoards with
            | [] -> Error "We ran out of boards"
            | [lastBoard] when Board.winning lastBoard ->
                (Board.sumOfUnmarked lastBoard) * head |> Ok
            | boards ->  //remove winners
                boards
                |> List.filter (fun b -> not (Board.winning b))
                |> findLastWinner tail

let printFirstWinner input =
    input
    |> Result.bind (fun (n, b) -> findFirstWinner n b )
    |> function
        | Error s -> printfn "Something went wrong: %s" s
        | Ok answer -> printfn "First winner has score: %i" answer

let printLastWinner input =
    input
    |> Result.bind (fun (n, b) -> findLastWinner n b )
    |> function
        | Error s -> printfn "Something went wrong: %s" s
        | Ok answer -> printfn "Last winner has score: %i" answer

[<EntryPoint>]
let main argv =
    argv
    |> List.ofArray
    |> function
        | [] -> printfn "Please provide input file"
        | file :: _tail ->
            input file
            |> fun data ->
                printFirstWinner data
                printLastWinner data
    0 // return an integer exit code
