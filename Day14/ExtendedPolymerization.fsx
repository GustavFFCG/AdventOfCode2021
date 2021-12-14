open System
open System.IO
open Microsoft.FSharp.Collections


type Insertion = {From:string;To:char}
type Indata = {
    Template: string
    Insertions: Insertion list
}

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

let toInput (strings: string seq) =
    { 
        Template = Seq.head strings
        Insertions =
            let removeStart (s: string) = s.Substring(11)
            strings
            |> Seq.filter (fun (s: string) -> s.Contains(" -> ") )
            |> Seq.map (fun (s: string) -> { From = s.Substring(0,2);To = s.[6] })
            |> List.ofSeq
    }

let input = 
    File.ReadLines(fileName)
    |> toInput

let addToFrequencyMap value m x =
    m 
    |> Map.tryFind x
    |> function
        | None -> m |> Map.add x value
        | Some i -> m |> Map.add x (i + value)

let rec toPairs state (s:string) =
    if s.Length = 1 then addToFrequencyMap 1L state s
    else 
        let newState = addToFrequencyMap 1L state (s.Substring(0,2))
        toPairs newState (s.Substring(1))

let insert (insertions:Insertion list) s =
    insertions |> List.tryFind (fun item -> item.From = s)
    |> function
        | Some insertion -> $"{s.[0]}{insertion.To}{s.[1]}"
        | None -> s

let toElementMap (segmentMap: Map<string,int64>) =
    segmentMap
    |> Map.toSeq
    |> Seq.map (fun (s,c) -> (s.[0],c) )
    |> Seq.fold (fun elementMap (c, count) -> addToFrequencyMap count elementMap c) Map.empty 

let getMaxAndMin m =
    m 
    |> Map.toList
    |> fun l ->
        {| 
            min = l |> List.minBy snd |> snd
            max = l |> List.maxBy snd |> snd
        |}

let step insertions m =
    let transformSegment (segment, count) = insert insertions segment, count
    let splitSegment ((segment: string), count) = if segment.Length = 3 then [(segment.Substring(0,2)), count; (segment.Substring(1)), count] else [segment, count]
    m 
    |> Map.toSeq
    |> Seq.map (transformSegment >> splitSegment)
    |> Seq.concat
    |> Seq.fold (fun state (segment, count) -> addToFrequencyMap count state segment) Map.empty

let initialState = input.Template |> toPairs Map.empty

let iterate nOfSteps =
    seq{1..nOfSteps}
    |> Seq.fold (fun state _i ->
        state
        |> step input.Insertions
        ) initialState
    |> toElementMap
    |> getMaxAndMin
    |> fun x -> x.max - x.min
    |> printfn "Diff after %i iterations: %i" nOfSteps

iterate 10
iterate 40
