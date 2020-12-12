namespace AdventOfCode

open System
open System.Diagnostics.PerformanceData

module Day10B =
    
    /// <summary>
    /// Counts the length for all 1-jolt streaks.
    /// </summary>
    /// <example>
    /// for a jolt differences list like:
    /// [3 3 1 1 3 1 3 1 1 1 1]
    /// the function will return 
    /// [2, 1, 4]
    /// </example>
    let rec countOneJoltStreakLengths (joltDifferences : int list) (partialResult: int list) (currentStreakLength : int ) : int list =
        match joltDifferences with
            | [] -> partialResult
            | (joltDifference::rest) ->
                    if joltDifference = 1 then
                        countOneJoltStreakLengths rest partialResult (currentStreakLength + 1)
                    else
                        if currentStreakLength = 0 then 
                            countOneJoltStreakLengths rest partialResult 0
                        else
                            countOneJoltStreakLengths rest (currentStreakLength::partialResult) 0
            
    
    /// <summary>
    /// WARNING! only works for n = 1..4
    /// Counts the possibilities for skipping 0-n jolts  in a way that the adapter line will still be valid.
    /// The formula is:
    /// length(n) = length(n-1) + (n-1)
    /// (for 1 it's 1)
    /// </summary>
    /// <example>
    /// Case1
    /// Assume the jolts are:
    /// 0 3 4 7
    /// So the jolt differences:
    ///  3 1 3 -> there's only one possibility: keep the jolt.
    ///
    /// Case2
    /// Assume the jolts are:
    /// 1 4 5 6 7 10
    /// So the jolt differences:
    ///  3 1 1 1 3 -> The streak length of [1,1,1] is 3.
    /// Count of possibilities length(3) = length(2) + 2 = length(1) + 1 + 2 = 1 + 1+ 2 = 4
    /// </example>
    let rec numberOfPossibilitiesForASteakWithLength (streakLength : int) =
        if streakLength = 1 then
            1
        else
            (numberOfPossibilitiesForASteakWithLength ( streakLength - 1)) + (streakLength - 1)
    
    let main (input : string) : string =
        let jolts = 
            input 
                |> Parser.parseToRows 
                |> Seq.map int 
                |> Seq.toList
        
        let myAdapter = (List.max jolts) + 3
        let sortedExtendedJolts = (0::myAdapter::jolts) |> List.sort
        let joltDifferences = Day10A.collectJoltDifferences sortedExtendedJolts
        let oneJoltStreakLengths = countOneJoltStreakLengths joltDifferences [] 0
        
        oneJoltStreakLengths
            |> Seq.map numberOfPossibilitiesForASteakWithLength
            |> Seq.map int64
            |> Seq.reduce (*)
            |> string
