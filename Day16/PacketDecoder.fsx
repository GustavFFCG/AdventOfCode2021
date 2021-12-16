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
        let charToHex (c:char) =
            Int16.Parse(string c, NumberStyles.HexNumber)
        let intToBin (i:int16) =
            Convert.ToString(i,2).PadLeft(4,'0')
        let charToBin = charToHex >> intToBin

        strings
        |> Seq.head
        |> Seq.fold (fun state c -> $"{state}{(charToBin c)}" ) ""

    let binaryString = 
        File.ReadLines(fileName)
        |> toInput

type PacketType =
    | Literal of int
    | Operator
type Packet = {
    Version: int
    Type: PacketType
    SubPackets:Packet list
}

module Packet =
    let private parseBinary s =
        Convert.ToInt32(s, 2)

    let rec ofString (state:Packet list) (maxPackets:int option) (s:string): Packet list*string = 
        let rec parseLiteral (s:string) =
            let number, tail = s.Substring(1,4), s.Substring(5)
            if s.StartsWith("0") then
                {| value=number; tail=tail |}
            else
                {| value=(number + (parseLiteral tail).value); tail=tail |}

        let parseOperator (s:string) =
            try
                match s.Substring(0,1) with
                    | "0" ->
                        let length = s.Substring(1,15) |> parseBinary
                        {|
                            value= ofString [] None (s.Substring(16,length)) |> fst
                            tail=s.Substring(16 + length)
                        |}
                    | "1" ->
                        let nOfPackets = s.Substring(1,11) |> parseBinary
                        let value, tail = s.Substring(12) |> ofString [] (Some nOfPackets)
                        {|
                            value=value
                            tail=tail
                        |}
                    | _ -> failwith "illegal char"
            with ex -> failwith $"exception parsing {s} as operator: {ex}" 

        if s.Length < 6 || not (Seq.exists (fun c -> c='1') s) then state, ""
        else if maxPackets = Some 0 then state,s
        else
            let packetVersion = s.Substring(0, 3) |> parseBinary
            let packetType, subPackets, tail = 
                s.Substring(3, 3) 
                |> parseBinary
                |> function
                    | 4 -> 
                        let literalContent = s.Substring(6) |> parseLiteral
                        literalContent.value
                        |> parseBinary
                        |> Literal, [], literalContent.tail
                    | _ -> 
                        let operatorContent = s.Substring(6) |> parseOperator
                        Operator, operatorContent.value , operatorContent.tail
            let thisPacket = {
                Version = packetVersion
                Type = packetType
                SubPackets = subPackets
            }
            let newState = thisPacket :: state
            let newMaxPackets = maxPackets |> Option.map (fun i -> i - 1)
            ofString newState newMaxPackets tail

    let rec sumVersionNumbers (packets: Packet list) =
        packets
        |> List.sumBy (fun p -> p.Version + (sumVersionNumbers p.SubPackets))

Input.binaryString
|> Packet.ofString [] None
|> fst
|> Packet.sumVersionNumbers
|> printfn "Sum of versions: %i"