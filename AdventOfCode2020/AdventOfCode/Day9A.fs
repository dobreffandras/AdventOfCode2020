namespace AdventOfCode

open System
open AdventOfCode

module Day9A =
    
    let rec findFirstInvalidNumber (preambleCount : int) (numbers : int64 list) : int64 =
        let preamble = Seq.take preambleCount numbers
        let allPossibleSums : int64 list =
            Seq.allPairs preamble preamble
                |> Seq.filter (fun (x, y) -> not (x = y))
                |> Seq.map (fun (x, y) -> x + y)
                |> Seq.toList
        let currentNum = List.head (List.skip preambleCount numbers)

        if not (Seq.contains currentNum allPossibleSums) then
            currentNum
        else
            match numbers with
                | (_::tail) -> findFirstInvalidNumber preambleCount tail
                | [] -> raise (ArgumentException("There is no invalid number"))
    
    let main (input : string) : string =
        let numbers =
            input
                |> Parser.parseToRows
                |> Seq.map int64
                |> Seq.toList
        findFirstInvalidNumber 25 numbers 
            |> string

