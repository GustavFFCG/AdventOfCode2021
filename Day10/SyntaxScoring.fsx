open System
open System.IO
open Microsoft.FSharp.Collections

type Rowstatus =
    | Ok
    | Incomplete of char list
    | Corrupt of char

module Rowstatus =
    let scoreCorrupted = function
        | Incomplete _ -> 0
        | Ok -> 0
        | Corrupt ')' -> 3
        | Corrupt ']' -> 57
        | Corrupt '}' -> 1197
        | Corrupt '>' -> 25137
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


source
|> Seq.map Rowstatus.forString
|> Seq.sumBy Rowstatus.scoreCorrupted
|> printfn "Score for corrupt lines is %i"

source
|> Seq.map Rowstatus.forString
|> Seq.map Rowstatus.scoreIncomplete
|> Seq.choose id
|> Seq.sort
|> fun s -> 
    let middleIndex = (Seq.length s - 1) /2
    s |> Seq.item middleIndex
|> printfn "Score to fix incomplete is %i"
