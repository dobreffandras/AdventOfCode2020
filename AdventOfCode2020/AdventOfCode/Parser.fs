namespace AdventOfCode

open System

module Parser =
    
    let splitByBlankLine (input : string) : string [] =
        let sepString = Environment.NewLine + Environment.NewLine 
        input.Split([|sepString|], StringSplitOptions.RemoveEmptyEntries)

    let parseToRows(input : string) : string [] =
        input.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)

    let splitBySpace (row : string) : string [] =
        row.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)
    
