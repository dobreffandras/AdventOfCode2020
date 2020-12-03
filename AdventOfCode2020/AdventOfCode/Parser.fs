namespace AdventOfCode

open System

module Parser =
    let parseToRows(input : string) : string [] =
        input.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

