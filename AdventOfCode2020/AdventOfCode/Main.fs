module AdventOfCode.Main

open System

[<EntryPoint>]
let main argv =
    Console.WriteLine (Day7A.main Inputs.Day7.input)
    0