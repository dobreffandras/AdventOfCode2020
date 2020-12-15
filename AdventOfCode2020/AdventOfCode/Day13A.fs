namespace AdventOfCode

open System
open AdventOfCode
open AdventOfCode.Day2

module Day13A =
    
    module Parser =
        let parseBusLines (inputRow : string) =
            let lines = inputRow.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
            let isNumber = (Seq.forall Char.IsDigit)
            lines
                |> Seq.filter isNumber
                |> Seq.map Int32.Parse
                
    
    let minsWaitingForBusLine busFrequency myArrival =
        ((myArrival / busFrequency + 1) * busFrequency - myArrival)
        
    let selectBestBus (myArrival: int) (busLines : seq<int>) : int*int =
        busLines
            |> Seq.map (fun b -> (b, (minsWaitingForBusLine b myArrival)))
            |> Seq.minBy (fun (_, waitTime) -> waitTime)
    
    let main input =
        let rows = Parser.parseToRows input
        let myArrival = rows.[0] |> int
        let busLines = rows.[1] |> Parser.parseBusLines
        let (bestBus, waitTime) = selectBestBus myArrival busLines
        bestBus * waitTime |> string
        
