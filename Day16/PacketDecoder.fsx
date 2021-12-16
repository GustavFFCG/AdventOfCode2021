open System
open System.IO
open Microsoft.FSharp.Collections
open System.Globalization

module Input =
    let private fileName = 
        fsi.CommandLineArgs 
        |> List.ofArray
        |> function | _::s::_ -> s | _ -> failwith "Please provide a filename for input"

    let toBinaryString s =
        let charToHex (c:char) =
            Int16.Parse(string c, NumberStyles.HexNumber)
        let intToBin (i:int16) =
            Convert.ToString(i,2).PadLeft(4,'0')
        let charToBin = charToHex >> intToBin

        s |> Seq.fold (fun state c -> $"{state}{(charToBin c)}" ) ""

    let fromParam = 
        File.ReadLines(fileName)
        |> Seq.head
        |> toBinaryString

type OperatorType =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo
module OperatorType =
    let ofInt = function
        | 0 -> Sum
        | 1 -> Product
        | 2 -> Minimum
        | 3 -> Maximum
        | 5 -> GreaterThan
        | 6 -> LessThan
        | 7 -> EqualTo
        | i -> failwithf "invalid operatur type %i" i

type PacketType =
    | Literal of int64
    | Operator of OperatorType
type Packet = {
    Version: int
    Type: PacketType
    SubPackets:Packet list
}

module Packet =
    type ParseMode =
        | Single
        | Sequential
        | FixNumber of int

    let private parse32Binary s =
        Convert.ToInt32(s, 2)
    let private parse64Binary s =
        Convert.ToInt64(s, 2)

    let rec ofString (state:Packet list) parseMode (s:string): Packet list*string = 
        let rec parseLiteral (s:string) =
            // printfn "parse literal %s" s
            let number, tail = s.Substring(1,4), s.Substring(5)
            if s.StartsWith("0") then
                {| value=number; tail=tail |}
            else
                let nextLiteral = parseLiteral tail
                {| value=(number + nextLiteral.value); tail=nextLiteral.tail |}

        let parseOperator (s:string) =
            // printfn "parse operator %s" s
            match s.Substring(0,1) with
                | "0" ->
                    let length = s.Substring(1,15) |> parse32Binary
                    //printfn "Subpackets for %i bits" length
                    if s.Length < (length + 1) then failwithf "Cannot draw %i from %s" length s
                    let value, _tail = ofString [] Sequential (s.Substring(16,length))
                    {|
                        value=value
                        tail=s.Substring(length + 16)
                    |}
                | "1" ->
                    let nOfPackets = s.Substring(1,11) |> parse32Binary
                    //printfn "Parse %i subpackets" nOfPackets
                    let value, tail = s.Substring(12) |> ofString [] (FixNumber nOfPackets)
                    {|
                        value=value
                        tail=tail
                    |}
                | _ -> failwith "illegal char"

        if s.Length < 6 || not (Seq.exists (fun c -> c='1') s) then state, s
        else if parseMode = FixNumber 0 then state,s
        else
            let packetVersion = s.Substring(0, 3) |> parse32Binary
            let packetType, subPackets, tail = 
                s.Substring(3, 3) 
                |> parse32Binary
                |> function
                    | 4 -> 
                        let literalContent = s.Substring(6) |> parseLiteral
                        literalContent.value
                        |> parse64Binary
                        |> Literal, [], literalContent.tail
                    | n -> 
                        let operatorContent = s.Substring(6) |> parseOperator
                        OperatorType.ofInt n |> Operator, operatorContent.value , operatorContent.tail
            let thisPacket = {
                Version = packetVersion
                Type = packetType
                SubPackets = subPackets |> List.rev
            }
            let newState = thisPacket :: state
            let newParsemode = 
                parseMode |> function
                    | FixNumber i -> FixNumber (i - 1)
                    | other -> other
            ofString newState newParsemode tail

    let rec sumVersionNumbers (packets: Packet list) =
        packets
        |> List.sumBy (fun p -> p.Version + (sumVersionNumbers p.SubPackets))

    let rec value (packet: Packet) =
        match packet.Type with
            | Literal i -> i
            | Operator Sum          -> packet.SubPackets |> List.sumBy value
            | Operator Product      -> packet.SubPackets |> List.fold (fun state p -> state * (value p)) 1L
            | Operator Minimum      -> packet.SubPackets |> List.map value |> List.min
            | Operator Maximum      -> packet.SubPackets |> List.map value |> List.max
            | Operator GreaterThan  -> 
                packet.SubPackets
                |> function
                    | [a;b] when (value a) > (value b) -> 1L
                    | [a;b] -> 0L
                    | _ -> failwith "exactly two packets required"
            | Operator LessThan     ->
                packet.SubPackets
                |> function
                    | [a;b] when (value a) < (value b) -> 1L
                    | [a;b] -> 0L
                    | _ -> failwith "exactly two packets required"
            | Operator EqualTo      ->
                packet.SubPackets
                |> function
                    | [a;b] when (value a) = (value b) -> 1L
                    | [a;b] -> 0L
                    | _ -> failwith "exactly two packets required"


let packet = 
    Input.fromParam
    |> Packet.ofString [] Packet.ParseMode.Single
    |> fst

packet
|> Packet.sumVersionNumbers
|> printfn "Sum of versions: %i"

packet
|> List.head
|> Packet.value
|> printfn "Total value: %i"
