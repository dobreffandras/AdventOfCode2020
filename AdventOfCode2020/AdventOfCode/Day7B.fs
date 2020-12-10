namespace AdventOfCode

open System

module Day7B =
    
    type Bag = { name : string }
    
    type Child = { number : int; bag : Bag }
    
    type Rule =
        {
            parent : Bag
            children: Child []
        }

    type RuleSet(rules : Rule []) = 
        member this.calculateAllBags (parent : Bag) : int =
            let rule  = Seq.find (fun r -> r.parent = parent) rules
            let subBagsCount =
                rule.children
                    |> Seq.map (fun c -> c.number * this.calculateAllBags c.bag)
                    |> Seq.sum
                    
            1 + subBagsCount
        
    module Parser =
        
        let parseContainer (inputContainer : string) : Bag =
            let split = Parser.splitBySpace inputContainer
            let pattern = split.[0]
            let color = split.[1]
            { name = pattern + " " + color }
        
        let parseContainment (inputContainment : string) : Child =
            let split = Parser.splitBySpace inputContainment
            let number = Int32.Parse split.[0]
            let pattern = split.[1]
            let color = split.[2]
            let name = pattern + " " + color
            { number = number; bag = { name = name}}
            
        let parseRule (inputRule : string) : Rule =
            let relationShip =
                inputRule.Split([|"contain"|], StringSplitOptions.RemoveEmptyEntries)
            let container = parseContainer relationShip.[0]
            let rawContents = relationShip.[1]
            let contents =
                match rawContents with
                    | " no other bags." -> Array.empty
                    | contents ->
                        contents.Split([|","|], StringSplitOptions.RemoveEmptyEntries)
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
        let count = ruleSet.calculateAllBags { name = "shiny gold" }
        string (count - 1)