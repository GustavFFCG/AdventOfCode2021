open System
open System.IO
open Microsoft.FSharp.Collections

type Rowstatus =
    | Ok
    | Incomplete of char list
    | Corrupt of char

module Rowstatus =
    let scoreCorrupted = function
        | Incomplete _ -> None
        | Ok -> None
        | Corrupt ')' -> Some 3
        | Corrupt ']' -> Some 57
        | Corrupt '}' -> Some 1197
        | Corrupt '>' -> Some 25137
        | Corrupt other -> failwith $"Unexpected corrupt char {other}"

    let scoreIncomplete = function
        | Ok -> None
        | Corrupt _ -> None
        | Incomplete l ->
            let valueOf = function
                | '(' -> 1L 
                | '[' -> 2L 
                | '{' -> 3L 
                | '<' -> 4L
                | other -> failwith $"Unexpected incomplete char {other}"
            l |> List.fold (fun state c -> state * 5L + valueOf c) 0L |> Some

    let forString s =
        let rec checkSyntax openChunks s =
            match s, openChunks with
            | [], [] ->
                    Rowstatus.Ok
            | [], _ ->
                    Rowstatus.Incomplete openChunks
            | head::tail, chunkhead::chunkTail ->
                match head with
                | ')' when chunkhead = '(' -> checkSyntax chunkTail tail
                | ']' when chunkhead = '[' -> checkSyntax chunkTail tail
                | '}' when chunkhead = '{' -> checkSyntax chunkTail tail
                | '>' when chunkhead = '<' -> checkSyntax chunkTail tail
                | ')' -> Rowstatus.Corrupt ')'
                | ']' -> Rowstatus.Corrupt ']'
                | '}' -> Rowstatus.Corrupt '}'
                | '>' -> Rowstatus.Corrupt '>'
                | '(' -> checkSyntax ('(' :: openChunks) tail 
                | '[' -> checkSyntax ('[' :: openChunks) tail 
                | '{' -> checkSyntax ('{' :: openChunks) tail 
                | '<' -> checkSyntax ('<' :: openChunks) tail 
                | _ -> checkSyntax tail openChunks
            | head::tail, [] ->
                match head with
                | ')' -> Rowstatus.Corrupt ')'
                | ']' -> Rowstatus.Corrupt ']'
                | '}' -> Rowstatus.Corrupt '}'
                | '>' -> Rowstatus.Corrupt '>'
                | '(' -> checkSyntax ['('] tail
                | '[' -> checkSyntax ['['] tail
                | '{' -> checkSyntax ['{'] tail
                | '<' -> checkSyntax ['<'] tail
                | _ -> checkSyntax tail openChunks
        s
        |> List.ofSeq
        |> checkSyntax []

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let source = 
    File.ReadLines(fileName)

let middleValue l =
    let middleIndex = (Seq.length l - 1) /2
    l |> Seq.sort |> Seq.item middleIndex

let rowStatus = source |> Seq.map Rowstatus.forString

rowStatus
|> Seq.map Rowstatus.scoreCorrupted
|> Seq.choose id
|> Seq.sum 
|> printfn "Score for corrupt lines is %i"

rowStatus
|> Seq.map Rowstatus.scoreIncomplete
|> Seq.choose id
|> middleValue
|> printfn "Score to fix incomplete is %i"
