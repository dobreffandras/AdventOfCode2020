namespace AdventOfCode

open System


module Day9B =
    
    let rec findSum (candidate : int64 list) (rest : int64 list) (target:int64) : int64 list =
        let sum = List.sum candidate
        if sum = target then
            candidate
        elif sum > target then
            let newFront =
                match candidate with
                    | (_::tail) -> tail
                    | [] -> raise (ArgumentException("There are no numbers to sum"))
            let newWholeList = newFront @ rest
            let newCandidate = List.take 2 newWholeList
            let newRest = List.skip 2 newWholeList
            findSum newCandidate newRest target
        else
            let x =
                match rest with
                    | (hd::_) -> hd
                    | [] -> raise (ArgumentException("There are no numbers to sum"))
            let newCandidate = (candidate @ [x])
            let newRest = List.tail rest
            findSum newCandidate newRest target
    
    let findContinuousSum (numbers : int64 list) (invalidNumber : int64) : int64 list =
        let candidate = List.take 2 numbers
        let rest = List.skip 2 numbers
        findSum candidate rest invalidNumber
    
    let main (input : string) : string =
        let numbers =
            input
                |> Parser.parseToRows
                |> Seq.map int64
                |> Seq.toList
        let invalidNumber = Day9A.findFirstInvalidNumber 25 numbers
        printfn "%d" invalidNumber
        let listSummingToInvalidNumber = findContinuousSum numbers invalidNumber
        printfn "%A" listSummingToInvalidNumber
        let min = List.min listSummingToInvalidNumber
        let max = List.max listSummingToInvalidNumber
        string (min + max)
        

