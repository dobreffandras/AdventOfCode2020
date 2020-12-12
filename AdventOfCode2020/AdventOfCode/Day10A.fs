namespace AdventOfCode

module Day10A =
    
    let collectJoltDifferences (numbers : int list) : int list =
        numbers
            |> Seq.windowed 2 
            |> Seq.map (fun pair -> pair.[1] - pair.[0])
            |> Seq.toList

    let IsOneJoltCout (((joltType, _) : int*int)) : bool = joltType = 1
    let IsThreeJoltCout (((joltType, _) : int*int)) : bool = joltType = 3

    let main (input : string)= 
        let jolts = 
            input 
                |> Parser.parseToRows 
                |> Seq.map int 
                |> Seq.toList
        
        let myAdapter = (List.max jolts) + 3

        let sortedExtendedJolts = (0::myAdapter::jolts) |> List.sort            
        printfn "Jolts (containing my built-in adapter and charging outlet): %A" sortedExtendedJolts

        let joltDifferences = collectJoltDifferences sortedExtendedJolts
        printfn "Jolt differences: %A" joltDifferences
        let joltDifferenceCounts = List.countBy id joltDifferences
        printfn "Count of Jolt Differences: %A" joltDifferenceCounts
        let oneJoltDifferenceCount = snd (Seq.find IsOneJoltCout joltDifferenceCounts)
        let threeJoltDifferenceCount = snd (Seq.find IsThreeJoltCout joltDifferenceCounts)
        oneJoltDifferenceCount * threeJoltDifferenceCount |> string