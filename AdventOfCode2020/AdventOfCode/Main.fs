module AdventOfCode.Main

open System

[<EntryPoint>]
let main argv =
    Console.WriteLine (Day10A.main Inputs.Day10.input)
    0