module AdventOfCode.Main

open System

[<EntryPoint>]
let main argv =
    Console.WriteLine (Day5B.main Inputs.Day5.input)
    0