namespace AdventOfCode.Tests


open AdventOfCode
open AdventOfCode.Day7B
open Xunit


module Day7B =
    
    [<Theory>]
    [<InlineData("pale yellow bags contain 5 faded cyan bags, 4 dotted blue bags.")>]
    let ``Parses 1 line of rule correctly`` (rule) =
        let rule = Parser.parseRule rule 
        Assert.Equal(
            {
                parent = { name = "pale yellow"}
                children = [|
                    { number = 5; bag ={ name = "faded cyan" }};
                    { number = 4; bag = { name = "dotted blue" }}
                |]
            },
            rule)

    [<Theory>]
    [<InlineData("pale yellow bags contain no other bags.")>]
    let ``Parses 1 line of rule  having no children correctly`` (rule) =
        let rule = Parser.parseRule rule 
        Assert.Equal(
            {
                parent = { name = "pale yellow"}
                children = [||]
            },
            rule)

    
    [<Theory>]
    [<InlineData("pale yellow bags contain 5 faded cyan bags, 4 dotted blue bags.
                 dim crimson bags contain 2 dark silver bags, 5 plaid lavender bags, 5 pale gold bags.
                 faded lavender bags contain 2 clear aqua bags.")>]
    let ``Parses multiple lines of rules correctly`` (rules) =
        let rules = Parser.parseRules rules 
        let expectedRules = 
            [|{
                parent = { name = "pale yellow" }
                children = [|
                    { number = 5; bag = { name = "faded cyan"}};
                    { number = 4; bag = { name = "dotted blue"}}
                |]
            };{
                parent = {name = "dim crimson" }
                children = [|
                    { number = 2; bag = { name = "dark silver"}};
                    { number = 5; bag = { name = "plaid lavender"}}
                    { number = 5; bag = { name = "pale gold"}}
                |]
            };{
                parent = { name = "faded lavender" }
                children = [|
                    { number = 2; bag = { name = "clear aqua" }}
                |]
            }|]
        Assert.Equal<Rule []>(expectedRules, rules)
        
    [<Fact>]
    let ``RuleSet.calculateAllBags - returns 1 for leaf`` () =
        let ruleSet =
            RuleSet([|{
                parent = { name = "faded lavender" }
                children = [||]                
            }|])
        let bagCount = ruleSet.calculateAllBags { name = "faded lavender" }
        Assert.Equal(1, bagCount)

    [<Fact>]
    let ``RuleSet.calculateAllBags - returns 2 for parent and leaf`` () =
        let ruleSet =
            RuleSet([|{
                parent = { name = "faded lavender" }
                children = [||]                
            };{
                parent = { name = "dotted red" }
                children = [|{ number = 1; bag = { name = "faded lavender" }}|]                
            }|])
        let bagCount = ruleSet.calculateAllBags { name = "dotted red" }
        Assert.Equal(2, bagCount)
        
    [<Fact>]
    let ``RuleSet.calculateAllBags - returns 3 for parent and two leaves`` () =
        let ruleSet =
            RuleSet([|{
                parent = { name = "faded lavender" }
                children = [||]                
            };{
                parent = { name = "pale yellow" }
                children = [||]                
            };{
                parent = { name = "dotted red" }
                children = [|
                    { number = 1; bag = { name = "pale yellow" }}
                    { number = 1; bag = { name = "faded lavender" }}
                |]                
            }|])
        let bagCount = ruleSet.calculateAllBags { name = "dotted red" }
        Assert.Equal(3, bagCount)
        
    [<Fact>]
    let ``RuleSet.calculateAllBags - returns 3 for parent and leaf * 2`` () =
        let ruleSet =
            RuleSet([|{
                parent = { name = "faded lavender" }
                children = [||]                
            };{
                parent = { name = "dotted red" }
                children = [|{ number = 2; bag = { name = "faded lavender" }}|]                
            }|])
        let bagCount = ruleSet.calculateAllBags { name = "dotted red" }
        Assert.Equal(3, bagCount)
        
    [<Fact>]
    let ``returns 126 for sample input`` () =
        let calculatedCount = main Inputs.Day7.sampleB
        let expected = string 126
        Assert.Equal(expected, calculatedCount)
        
    