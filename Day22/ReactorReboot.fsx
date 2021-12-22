open System
open System.IO
open Microsoft.FSharp.Collections
open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern) in
    if m.Success then Some ([ for g in m.Groups -> g.Value ]) else None

type OnOff = | On | Off
type Coord = {X:int;Y:int;Z:int}
type RebootStep = {
    OnOff : OnOff
    Cubes: Coord Set
}
module RebootStep =
    let ofString = function
        | Regex "(on|off) x=(-?\d+)\.{2}(-?\d+),y=(-?\d+)\.{2}(-?\d+),z=(-?\d+)\.{2}(-?\d+)" l -> 
            match l with
                | _::onoff::xmin::xmax::ymin::ymax::zmin::zmax::_ ->
                    printfn "constructing step %s %s-%s %s-%s %s-%s" onoff xmin xmax ymin ymax zmin zmax
                    if int xmin >= -50 && int xmax <= 50 && int ymin >= -50 && int ymax <= 50 && int zmin >= -50 && int zmax <= 50 then
                        {
                            OnOff = if onoff = "on" then On else Off
                            Cubes = 
                                seq{int xmin..int xmax}
                                |> Seq.map (fun x -> 
                                    seq{int ymin..int ymax }
                                    |> Seq.map (fun y ->
                                        seq{int zmin..int zmax}
                                        |> Seq.map (fun z -> {X=x;Y=y;Z=z})
                                        )
                                    )
                                |> Seq.concat |> Seq.concat
                                |> Set.ofSeq
                                |> fun x -> printfn "%i elements" x.Count ; x
                        } |> Some
                    else None
                | other -> failwith $"Unexpected input {other}"
        | other -> failwith $"Unexpected input {other}"

    let in50Cube (step:RebootStep) =
        step.Cubes
        |> Set.exists (fun c -> c.X < -50 || c.X > 50 || c.Y < -50 || c.Y > 50 || c.Z < -50 || c.Z > 50)
        |> not

    let apply (step: RebootStep) (set: Coord Set) =
        match step.OnOff with
            | On -> Set.union set step.Cubes
            | Off -> Set.difference set step.Cubes

module Input =
    let fileName = 
        fsi.CommandLineArgs 
        |> List.ofArray
        |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

    let source = 
        File.ReadLines(fileName)

    let toInput strings =
        strings |> Seq.fold (fun state s -> RebootStep.ofString s :: state) [] |> List.rev


Input.source
|> Input.toInput
|> fun x -> printfn "Input count %i" x.Length ; x
|> List.choose id
|> fun x -> printfn "Filtered input count %i" x.Length ; x
|> List.fold (fun state step -> 
    RebootStep.apply step state
    ) Set.empty
|> Set.count
|> printfn "Set contains %i lit cubes"