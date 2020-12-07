namespace AdventOfCode

open AdventOfCode.Day2
open AdventOfCode.Day5A

module Day6A =
    
    type Group =
        { questionsAnsweredYes : char[] }
    
    module Parser =
        
        let toGroup (input : string) : Group =
            let chars =
                Parser.parseToRows input
                    |> Seq.concat
                    |> Seq.distinct
                    |> Seq.toArray

            { questionsAnsweredYes = chars }
        
        let parseGroups (input : string) : seq<Group> =
            Parser.splitByBlankLine input
                |> Seq.map toGroup
    
    let main (input : string) : string =
        let groups = Parser.parseGroups input

        groups
            |> Seq.map (fun g -> g.questionsAnsweredYes.Length)
            |> Seq.reduce (+)
            |> string
