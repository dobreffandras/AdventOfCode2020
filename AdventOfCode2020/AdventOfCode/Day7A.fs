namespace AdventOfCode

open System

module Day7A =
    
    type Rule =
        {
            parent : string
            children: string []
        }

    type RuleSet(rules : Rule []) = 
        let rulesHavingChild (child : string) : seq<Rule> = 
            rules |> Seq.filter (fun r -> Seq.contains child r.children)
            
        member this.searchParents (child: string) : Set<string> =             
             let rulesHavingThatChild = (rulesHavingChild child)
             if Seq.isEmpty rulesHavingThatChild  then
                 Set.empty
             else
                 rulesHavingThatChild
                     |> Seq.map (fun r -> ((this.searchParents r.parent).Add(r.parent))) 
                     |> Seq.concat
                     |> set
        
    module Parser =
        
        let parseContainer (inputContainer : string) : string =
            let split = Parser.splitBySpace inputContainer
            let pattern = split.[0]
            let color = split.[1]
            pattern + " " + color
        
        let parseContainment (inputContainment : string) : string =
            let split = Parser.splitBySpace inputContainment
            let pattern = split.[1]
            let color = split.[2]
            pattern + " " + color
            
        let parseRule (inputRule : string) : Rule =
            let relationShip = inputRule.Split([|"contain"|], StringSplitOptions.RemoveEmptyEntries)
            let container = parseContainer relationShip.[0]
            let rawContents = relationShip.[1]
            let contents =
                rawContents.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
                |> Seq.map parseContainment
                |> Seq.toArray
            
            {
                parent = container
                children = contents
            }
        
        let parseRules (inputRules : string) : Rule [] =
            Parser.parseToRows inputRules
                |> Seq.map parseRule
                |> Seq.toArray
        
    
    
    let main (input : string) : string =
        let ruleSet = RuleSet(Parser.parseRules input)
        (ruleSet.searchParents "shiny gold")
            |> Seq.length
            |> string
