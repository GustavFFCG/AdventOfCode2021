open System
open System.IO
open Microsoft.FSharp.Collections
open System.Globalization

module Input =
    let private fileName = 
        fsi.CommandLineArgs 
        |> List.ofArray
        |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

    let private toInput (strings: string seq) =
        strings
        |> Seq.head
        |> Seq.fold (fun state c -> 
            state + (Int16.Parse(c, NumberStyles.HexNumber) |> sprintf "%c=%i " c )  ) ""

    let binaryString = 
        File.ReadLines(fileName)
        |> toInput

Input.binaryString
|> printfn "%s"