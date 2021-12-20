open System
open System.IO
open Microsoft.FSharp.Collections

type Coord = {x: int; y:int}
    with static member ofxy x y = {x=x; y=y}

type Image = {
    map: Map<Coord,bool>
    defaultValue: bool
}
type EnhancementAlgorithm = bool list

module Input =
    let boolOfChar = function | '#' -> true | '.' -> false | _ -> failwith "unexpected input"
    let fileName = 
        fsi.CommandLineArgs 
        |> List.ofArray
        |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

    let ofFile strings =
        let algorithm s : EnhancementAlgorithm = 
            s |> Seq.fold (fun state c ->
                (boolOfChar c) :: state
            ) []
            |> List.rev
        let image strings : Image =
            {   
                map =
                    strings
                    |> List.mapi (fun y s ->
                        s |> List.ofSeq |> List.mapi (fun x c -> (Coord.ofxy x y), boolOfChar c
                        )
                    )
                    |> List.concat
                    |> Map.ofList
                defaultValue = false
            }

        strings |> List.ofSeq
        |> function
            | algorithmRow::blankrow::imageRows when blankrow = "" ->
                {| Algorithm = (algorithm algorithmRow); Image= image imageRows |}
            | _ -> failwith "Unexpected input"

    let data = 
        File.ReadLines fileName
        |> ofFile 

module EnhancementAlgorithm =
    let value a i = a |> List.item i
    let maxValue a = value a 511

module Image =
    let ninePxArea coord =
        [
            coord.x-1, coord.y-1; coord.x, coord.y-1; coord.x+1, coord.y-1
            coord.x-1, coord.y;   coord.x, coord.y;   coord.x+1, coord.y
            coord.x-1, coord.y+1; coord.x, coord.y+1; coord.x+1, coord.y+1
        ]
        |> List.map (fun (x,y) -> Coord.ofxy x y)

    let isLit coord image =
        image.map |> Map.tryFind coord |> Option.defaultValue image.defaultValue

    let areaToInt image coord = 
        coord |> ninePxArea
        |> List.fold (fun state coord -> 
            sprintf "%s%c" state (if isLit coord image then '1' else '0') ) ""
        |> fun s -> Convert.ToInt32(s,2)

    let toString size (image:Image) =
        seq {-size..size}
        |> Seq.map ( fun y -> 
            seq {-size..size}
            |> Seq.map (fun x ->
                if isLit (Coord.ofxy x y) image then '#' else '.'
            )
            |> String.Concat
            |> fun s -> s + Environment.NewLine
        ) 
        |>Seq.fold (fun state s -> state + s) ""

    let step (algorithm:EnhancementAlgorithm) (image:Image) = 
        let coordsToEnhance : Coord Set =
            image.map
            |> Map.fold (fun state c _b ->
                c |> ninePxArea |> Set.ofList |> Set.union state
                ) Set.empty
        let newMap =         
            coordsToEnhance
            |> Set.fold (fun state c ->
                c |> areaToInt image |> EnhancementAlgorithm.value algorithm
                |> fun x -> state |> Map.add c x
                ) Map.empty
        let newDefaultValue = 
            if image.defaultValue 
            then EnhancementAlgorithm.maxValue algorithm 
            else EnhancementAlgorithm.value algorithm 0
        {
            map = newMap
            defaultValue = newDefaultValue
        }

    let nOfLit (image:Image) =
        if image.defaultValue 
        then Error "Infinite count" 
        else image.map |> Map.fold (fun state _key b -> if b then state + 1 else state) 0 |> Ok

let input = Input.data

seq{1..50}
|> Seq.fold (fun state _i -> 
    Image.step input.Algorithm state ) input.Image
|> Image.nOfLit
|> function
    | Ok i -> printfn "We have %i pixels lit" i
    | Error s -> printfn "%s" s