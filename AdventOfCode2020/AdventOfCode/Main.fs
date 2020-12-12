module AdventOfCode.Main

open System

[<EntryPoint>]
let main argv =
    Console.WriteLine (Day10B.main Inputs.Day10.input)
    0