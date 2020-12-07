namespace AdventOfCode

open AdventOfCode.Day2
open AdventOfCode.Day5A

module Day6B =
    
    type Group =
        { questionsEveryOneAnsweredYes : char[] }
    
    module Parser =
        
        let toGroup (input : string) : Group =
            let chars =
                Parser.parseToRows input
                    |> Seq.map (fun answers -> Set(answers))
                    |> Seq.reduce (fun x y -> Set.intersect x y)
                    |> Seq.toArray

            { questionsEveryOneAnsweredYes = chars }
        
        let parseGroups (input : string) : seq<Group> =
            Parser.splitByBlankLine input
                |> Seq.map toGroup
    
    let main (input : string) : string =
        let groups = Parser.parseGroups input

        groups
            |> Seq.map (fun g -> g.questionsEveryOneAnsweredYes.Length)
            |> Seq.reduce (+)
            |> string
