namespace AdventOfCode.Tests

open AdventOfCode.Day6A
open Xunit

module Day6A =
    
    [<Fact(Skip = "somehow not running on my machine")>] 
    let ``Should parse into groups`` =
        let groups = Parser.parseGroups AdventOfCode.Inputs.Day6.sample
        let expected : Group [] = [||]
        Assert.Equal<seq<Group>>(expected, groups)