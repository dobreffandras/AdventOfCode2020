namespace AdventOfCode.Tests

open Xunit
open AdventOfCode.Day11A
open Xunit

module Day11A =
    
    [<Fact>]
    let ``Parses ferry with single floor position`` () =
        let parsed = Parser.parseFerrySeats "."
        Assert.Equal(array2D [[Floor]], parsed)

    [<Fact>]
    let ``Parses ferry with single empty position`` () =
        let parsed = Parser.parseFerrySeats "L"
        Assert.Equal(array2D [[Empty]], parsed)

    [<Fact>]
    let ``Parses ferry with single occupied position`` () =
        let parsed = Parser.parseFerrySeats "#"
        Assert.Equal(array2D [[Occupied]], parsed)

    [<Fact>]
    let ``Parses a 4x4 ferry`` () =
        let parsed = Parser.parseFerrySeats "L.
LL"
        Assert.Equal(array2D [[Empty; Floor]; [Empty; Empty]], parsed)

    [<Fact>]
    let ``nextFerrySetup creates a setup with all occupied from all empty`` () =
        let initial =
            [[Empty; Floor; Floor];
            [Empty; Empty; Floor];
            [Empty; Empty; Floor]] |> array2D
        let expected =
            [[Occupied; Floor; Floor];
            [Occupied; Occupied; Floor];
            [Occupied; Occupied; Floor]] |> array2D
            
        let result = nextFerrySetup initial
        Assert.Equal(expected, result)

    [<Fact>]
    let ``nextFerrySetup doesn't occupy seat when it has an occupied neighbour`` () =
        let initial =
            [[Floor; Floor; Floor];
            [Floor; Empty; Floor];
            [Floor; Occupied; Floor]] |> array2D
        let expected =
            [[Floor; Floor; Floor];
            [Floor; Empty; Floor];
            [Floor; Occupied; Floor]]  |> array2D
            
        let result = nextFerrySetup initial
        Assert.Equal(expected, result)
        
    [<Fact>]
    let ``nextFerrySetup leaves occupied seat when there are more than 4 occupied neighbours`` () =
        let initial =
            [[Floor; Floor; Floor];
            [Occupied; Empty; Occupied];
            [Occupied; Occupied; Occupied]] |> array2D
        let expected =
            [[Floor; Floor; Floor];
            [Occupied; Empty; Occupied];
            [Occupied; Empty; Occupied]]  |> array2D
            
        let result = nextFerrySetup initial
        Assert.Equal(expected, result)
        
    [<Fact>]
    let ``occupySeats terminates with correct setup`` () =
        let initial =
            [[Floor; Floor; Floor];
            [Empty; Empty; Empty];
            [Empty; Empty; Empty]] |> array2D
        let expected =
            [[Floor; Floor; Floor];
            [Occupied; Empty; Occupied];
            [Occupied; Empty; Occupied]]  |> array2D
            
        let result = occupySeats initial
        Assert.Equal(expected, result)
    
    [<Fact>]
    let ``main runs occupySeatsCorrectly for sample input`` () =
        let result = main AdventOfCode.Inputs.Day11.sample 
        Assert.Equal("37", result)
        
    [<Fact>]
    let ``main runs occupySeatsCorrectly for puzzle input`` () =
        let result = main AdventOfCode.Inputs.Day11.input 
        Assert.Equal("2281", result)
    
    