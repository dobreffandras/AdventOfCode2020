namespace AdventOfCode.Tests

open AdventOfCode
open Xunit

module Day8A =
    
    [<Fact>]
    let ``returns 0 for empty program`` () =
        let result = Day8A.main ""
        Assert.Equal(0, result)

    [<Fact>]
    let ``returns 0 for a program having a "nop +0"`` () =
        let result = Day8A.main "nop +0"
        Assert.Equal(0, result)

    [<Fact>]
    let ``returns 1 for a program having an "acc +1"`` () =
        let result = Day8A.main "acc +1"
        Assert.Equal(1, result)
        
    [<Fact>]
    let ``returns 2 for a program having an "acc +2"`` () =
        let result = Day8A.main "acc +2"
        Assert.Equal(2, result)
        
    [<Fact>]
    let ``returns -2 for a program having an "acc -2"`` () =
        let result = Day8A.main "acc -2"
        Assert.Equal(-2, result)
        
    [<Fact>]
    let ``runs for a program having a "nop" and "acc +2"`` () =
        let result = Day8A.main "nop +0
        acc +2"
        Assert.Equal(2, result)

    [<Fact>]
    let ``return 3 for a program having a "acc +1" and "acc +2"`` () =
        let result = Day8A.main "acc +1
        acc +2"
        Assert.Equal(3, result)

    [<Fact>]
    let ``return 3 for a program having a "acc +1" a "nop" and "acc +2"`` () =
        let result = Day8A.main "acc +1
        nop +0
        acc +2"
        Assert.Equal(3, result)
    
    [<Fact>]
    let ``"jmp +2" skips the next instruction"`` () =
        let result = Day8A.main "jmp +2
        acc +1
        acc +2"
        Assert.Equal(2, result)
    
    [<Fact>]
    let ``"jmp -2" jumps back in program"`` () =
        let result = Day8A.main "jmp +3
        acc +5
        jmp +2
        jmp -2
        nop +0"
        Assert.Equal(5, result)

    [<Fact>]
    let ``terminates when infinite loop is detected"`` () =
        let result = Day8A.main "nop +0
        jmp -1"
        Assert.Equal(0, result)

    [<Fact>]
    let ``returns result before right before infinite loop is detected"`` () =
        let result = Day8A.main "nop +0
        acc +5
        jmp -1"
        Assert.Equal(5, result)

    [<Fact>]
    let ``returns correctly for sample input"`` () =
        let result = Day8A.main Inputs.Day8.sampleA
        Assert.Equal(5, result)

