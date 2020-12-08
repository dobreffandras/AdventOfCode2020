namespace AdventOfCode.Tests


open AdventOfCode
open AdventOfCode.Day7A
open Xunit


module Day7A =
    
    [<Theory>]
    [<InlineData("pale yellow bags contain 5 faded cyan bags, 4 dotted blue bags.")>]
    let ``Parses 1 line of rule correctly`` (rule) =
        let rule = Parser.parseRule rule 
        Assert.Equal(
            {
                parent = "pale yellow"
                children = [|"faded cyan"; "dotted blue"|]
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
                parent = "pale yellow"
                children = [|"faded cyan"; "dotted blue"|]
            };{
                parent = "dim crimson"
                children = [|"dark silver"; "plaid lavender"; "pale gold"|]
            };{
                parent = "faded lavender"
                children = [|"clear aqua"|]
            }|]
        Assert.Equal<Rule []>(expectedRules, rules)

    [<Fact>]
    let ``searchParents - can find parent of a child based on one rule``() =
        let ruleSet =
            RuleSet([|{
                parent = "faded lavender"
                children = [|"clear aqua"|]
            }|])
        let parents = ruleSet.searchParents "clear aqua"
        
        Assert.Equal<Set<string>>(set ["faded lavender"], parents)

    [<Fact>]
    let ``searchParents - can find parents of a child based on multiple rules``() =
        let ruleSet =
            RuleSet([|{
                parent = "faded lavender"
                children = [|"clear aqua"|]
            };{
                parent = "dotted red"
                children = [|"clear aqua"|]
            }|])
        let parents = ruleSet.searchParents "clear aqua"
        
        Assert.Equal<Set<string>>( set ["faded lavender"; "dotted red"], parents)
   
    [<Fact>]
    let ``searchParents - doesn't find parent of other children based on multiple rules``() =
        let ruleSet =
            RuleSet([|{
                parent = "faded lavender"
                children = [|"clear aqua"|]
            };{
                parent = "dotted red"
                children = [|"clear aqua"|]
            };{
                parent = "dotted red"
                children = [|"pale blue"|]
            }|])
        let parents = ruleSet.searchParents "clear aqua"
        
        Assert.Equal<Set<string>>( set ["faded lavender"; "dotted red"], parents)
        
        
    [<Fact>]
    let ``searchParents - can find grandparent of a child based on multiple rules``() =
        let ruleSet =
            RuleSet([|{
                parent = "faded lavender"
                children = [|"clear aqua"|]
            };{
                parent = "dotted red"
                children = [|"clear aqua"|]
            };{
                parent = "pale blue"
                children = [|"dotted red"|]
            }|])
        let parents = ruleSet.searchParents "clear aqua"
        
        Assert.Equal<Set<string>>( set ["faded lavender"; "dotted red"; "pale blue"], parents)

    [<Fact>]
    let ``searchParents - contains every parent only once`` () =
        let ruleSet =
            RuleSet([|{
                    parent = "faded lavender"
                    children = [|"clear aqua"|]
                };{
                    parent = "dotted red"
                    children = [|"clear aqua"|]
                };{
                    parent = "pale blue"
                    children = [|"dotted red"; "faded lavender"|]
                }|])
        let parents = ruleSet.searchParents "clear aqua"
        Assert.Equal<Set<string>>( set ["faded lavender"; "dotted red"; "pale blue"], parents)
        
        