module AdventOfCode.Main

open System

[<EntryPoint>]
let main argv =
    Console.WriteLine (Day9A.main Inputs.Day9.input)
    0