open CommonHelpers

exception OperationError of string
type PacketType =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo
    | LiteralValue

type PacketHeader = { Type: PacketType; Version: uint64 }

type Packet =
    | ValuePacket of header: PacketHeader * value: uint64
    | OperatorPacket of header: PacketHeader * subPackets: seq<Packet>

type PacketContainerType =
    | BitLength
    | PacketLength
let PacketHeaderLength = 3 + 3

let decodeHexDigit hexDigit = System.Int32.Parse(hexDigit, System.Globalization.NumberStyles.HexNumber)
let intToBits i =
    [|
        (i &&& 0b1000 >>> 3) |> byte
        (i &&& 0b0100 >>> 2) |> byte
        (i &&& 0b0010 >>> 1) |> byte
        (i &&& 0b0001 >>> 0) |> byte
    |]
    
let hexStringToBits hexString =
    hexString
    |> Helpers.charStrings
    |> Seq.map decodeHexDigit
    |> Seq.collect intToBits
    |> Seq.toArray
    
let bitStringToString (bits: seq<byte>) =
    bits
    |> Seq.map (fun b -> if b = (byte 1) then "1" else "0")
    |> String.concat ""

let bitStringToInt (bits: array<byte>) =
    let reversed = bits |> Array.rev
    
    let mutable r = 0uL
    for i = 0 to (reversed.Length - 1) do
        r <- r + ((uint64 reversed[i]) <<< i)
    r

let intToPacketType (i: uint64) =
    match i with
    | 0uL -> PacketType.Sum
    | 1uL -> PacketType.Product
    | 2uL -> PacketType.Minimum
    | 3uL -> PacketType.Maximum
    | 4uL -> PacketType.LiteralValue
    | 5uL -> PacketType.GreaterThan
    | 6uL -> PacketType.LessThan
    | 7uL -> PacketType.EqualTo
    | _ -> raise (OperationError($"Invalid packet type {i}"))

let decodePacketHeader (bits: array<byte>) =
    let versionBits = bits |> Array.take 3
    let typeBits = bits |> Array.skip 3 |> Array.take 3
    { Version = (bitStringToInt versionBits); Type = (typeBits |> bitStringToInt |> intToPacketType) }

    
let decodeOperatorPacketType (bits: array<byte>) =
    match (bits |> Array.skip PacketHeaderLength |> Array.item 0) with
    | 0uy -> PacketContainerType.BitLength
    | _ -> PacketContainerType.PacketLength

    
let rec decodePacket (bits: array<byte>) =
    let header = decodePacketHeader bits
    match header.Type with
    | LiteralValue -> decodeLiteralValuePacket bits
    | _ -> decodeOperatorPacket bits
    
and decodeLiteralValuePacket (bits: array<byte>) =
    let header = decodePacketHeader bits
    let bits = bits |> Seq.skip (PacketHeaderLength |> int)
    
    let valueChunks = bits
                    |> Seq.chunkBySize 5// values are in chunks of five
                    |> Helpers.takeUntil  (fun b -> (Seq.item 0 b) <> 1uy) // take until a chunk doesn't start with 1 (but take that first non-one chunk too)
                    
    let valueBits = valueChunks
                    |> Seq.map (Seq.skip 1)
                    |> Seq.collect id
                    |> Seq.toArray
    
    let packetLength = PacketHeaderLength + ((valueChunks |> Seq.length)  * 5)
    
    (packetLength |> uint64, ValuePacket(header = header, value = (bitStringToInt valueBits)))
    
and decodeOperatorPacket (bits: array<byte>) =
    let header = decodePacketHeader bits
    let operatorPacketType = decodeOperatorPacketType bits
    let (subPacketLengths, subPackets) = decodeSubPackets operatorPacketType (bits |> Array.skip (PacketHeaderLength + 1))
    
    ((uint64 PacketHeaderLength) + 1uL + subPacketLengths, OperatorPacket(header = header, subPackets = subPackets))
    
// bits should not contain the header or length type id bits of the parent packet
and decodeSubPackets operatorPacketType (bits: array<byte>) : uint64 * array<Packet> =
    match operatorPacketType with
    | PacketContainerType.BitLength -> decodeBitLengthSubPackets bits
    | PacketContainerType.PacketLength -> decodePacketLengthSubPackets bits

// bits should not contain the header or length type id bits of the parent packet
and decodeBitLengthSubPackets (bits: array<byte>) =
    let slowVersion bits =
        let subPacketsLength = bits |> Array.take 15 |> bitStringToInt
        let subPacketBits = bits |> Array.skip 15 |> Array.take (int subPacketsLength)
        
        let packetsAndLengths = seq {
            let mutable subPacketBits = subPacketBits
            while (subPacketBits |> (Seq.isEmpty >> not)) do
                let (nextLength, nextPacket) = decodePacket(subPacketBits)
                subPacketBits <- subPacketBits |> Array.skip (int nextLength)
            
                yield (nextLength, nextPacket)
        }
        
        let totalLength = 15uL + (packetsAndLengths |> Seq.map fst |> Seq.sum)
        let packets = packetsAndLengths |> Seq.map snd |> Seq.toArray

        (totalLength, packets)
        
    let recursiveVersion bits =
        let subPacketsLength = bits |> Array.take 15 |> bitStringToInt
        let subPacketBits = bits |> Array.skip 15 |> Array.take (int subPacketsLength)
        
        let rec inner remainingBits (acc: uint64 * array<Packet>) =
            if (Array.isEmpty remainingBits) then
                acc
            else
                let (previousLength, previousPackets) = acc
                let (nextLength, nextPacket) = decodePacket remainingBits
                let newBits = remainingBits |> Array.skip (int nextLength)
                let newLength = previousLength + nextLength
                let newPackets = Array.append previousPackets [| nextPacket |]
                
                inner newBits (newLength, newPackets)

        inner subPacketBits (15uL, [||])
            
    let fastVersion bits =
        let subPacketsLength = bits |> Array.take 15 |> bitStringToInt
        let mutable subPacketBits = bits |> Array.skip 15 |> Array.take (int subPacketsLength)
        
        let mutable totalLength = 15uL
        let packets = System.Collections.Generic.List<Packet>()
        while (subPacketBits |> (Array.isEmpty >> not)) do
            let (nextLength, nextPacket) = decodePacket(subPacketBits)
            subPacketBits <- subPacketBits |> Array.skip (int nextLength)
            
            totalLength <- totalLength + nextLength
            packets.Add(nextPacket)

        (totalLength, packets.ToArray())
        
    //slowVersion bits
    //fastVersion bits
    recursiveVersion bits

// bits should not contain the header or length type id bits of the parent packet
and decodePacketLengthSubPackets (bits: array<byte>) =
    let slowVersion bits =
        let subPacketCount = bits |> Array.take 11 |> bitStringToInt |> int
        let subPacketBits = bits |> Array.skip 11
        let packetsAndLengths = seq {
            let mutable subPacketBits = subPacketBits
            for i = 1 to subPacketCount do
                let (nextLength, nextPacket) = decodePacket(subPacketBits)
                subPacketBits <- subPacketBits |> Array.skip (int nextLength)
                
                yield (nextLength, nextPacket)
        }
        let totalLength = 11uL + (packetsAndLengths |> Seq.map fst |> Seq.sum)
        let packets = packetsAndLengths |> Seq.map snd |> Seq.toArray
        (totalLength, packets)

    let recursiveVersion bits =
        let subPacketCount = bits |> Array.take 11 |> bitStringToInt |> int
        let subPacketBits = bits |> Array.skip 11
        
        let rec inner remainingCount remainingBits (acc: uint64 * array<Packet>) =
            if remainingCount = 0 then
                acc
            else    
                let (previousLength, previousPackets) = acc
                let (nextLength, nextPacket) = decodePacket remainingBits
                let newBits = remainingBits |> Array.skip (int nextLength)
                let newLength = previousLength + nextLength
                let newPackets = Array.append previousPackets [| nextPacket |]

                inner (remainingCount - 1) newBits (newLength, newPackets)

        inner subPacketCount subPacketBits (11uL, [||])

    let fastVersion bits =
        let subPacketCount = bits |> Array.take 11 |> bitStringToInt |> int
        let mutable subPacketBits = bits |> Array.skip 11
        let mutable totalLength = 11uL
        
        let packets = System.Collections.Generic.List<Packet>()
        for i = 1 to subPacketCount do
            let (nextLength, nextPacket) = decodePacket subPacketBits
            subPacketBits <- subPacketBits |> Array.skip (int nextLength)
            totalLength <- totalLength + nextLength
            packets.Add(nextPacket)
        (totalLength, packets.ToArray())
      
    //slowVersion bits  
    //fastVersion bits
    recursiveVersion bits
    
let rec versionSum (packet: Packet) =
    match packet with
    | ValuePacket (header, _) -> header.Version
    | OperatorPacket (header, subPackets) -> header.Version + (subPackets |> Seq.map versionSum |> Seq.sum)
    
let rec packetValue (packet: Packet) =
    match packet with
    | ValuePacket (_, value) -> value
    | OperatorPacket (header, subPackets) ->
        let op = match header.Type with
                 | Sum -> Seq.sum
                 | Product -> Seq.reduce (fun a b -> a * b)
                 | Minimum -> Seq.min
                 | Maximum -> Seq.max
                 | GreaterThan -> (fun s -> if (Seq.item 0 s) > (Seq.item 1 s) then 1uL else 0uL)
                 | LessThan -> (fun s -> if (Seq.item 0 s) < (Seq.item 1 s) then 1uL else 0uL)
                 | EqualTo -> (fun s -> if (Seq.item 0 s) = (Seq.item 1 s) then 1uL else 0uL)
                 | _ -> raise (OperationError("Could not determine operator"))
        subPackets |> Seq.map packetValue |> op

let part1 path hexString =
    let answer = hexString
                 |> hexStringToBits
                 |> decodePacket
                 |> snd
                 |> versionSum

    printfn  " │ "
    printfn  " │ Part 1: "
    
    if (System.String.IsNullOrWhiteSpace(path) |> not) then
        printfn $" │ %s{path}"
    else ()
    
    printfn $" │ %s{hexString}"
    printfn  " │ "
    printfn $" │ Version Sum: %u{answer}"
    printfn  " └────────────"

let part2 path hexString =
    let answer = hexString
                 |> hexStringToBits
                 |> decodePacket
                 |> snd
                 |> packetValue

    printfn  " │ "
    printfn  " │ Part 2: "
    
    if (System.String.IsNullOrWhiteSpace(path) |> not) then
        printfn $" │ %s{path}"
    else ()
    
    printfn $" │ %s{hexString}"
    printfn  " │ "
    printfn $" │ Packet Value: %u{answer}"
    printfn  " └────────────"
    
"D2FE28"|> part1 ""
"38006F45291200"|> part1 ""
"EE00D40C823060"|> part1 ""
"8A004A801A8002F478" |> part1 ""
"620080001611562C8802118E34" |> part1 ""
"C0015000016115A2E0802F182340" |> part1 ""
"A0016C880162017C3686B18A3D4780" |> part1 ""

System.IO.File.ReadAllText("input.txt").Trim() |> part1 "input.txt"

"C200B40A82" |> part2 ""
"04005AC33890" |> part2 ""
"880086C3E88112" |> part2 ""
"CE00C43D881120" |> part2 ""
"D8005AC2A8F0" |> part2 ""
"F600BC2D8F" |> part2 ""
"9C005AC2F8F0" |> part2 ""
"9C0141080250320F1802104A08" |> part2 ""

System.IO.File.ReadAllText("input.txt").Trim() |> part2 "input.txt"