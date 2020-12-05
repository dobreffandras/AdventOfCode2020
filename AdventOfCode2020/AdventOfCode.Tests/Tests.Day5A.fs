namespace AdventOfCode.Tests

open AdventOfCode.Day5A
open Xunit

module Day5A =
    
    [<Theory>]
    [<InlineData("FFFBBBFRRR")>]    
    [<InlineData("BFFFBBFRRR")>]    
    [<InlineData("FBFBBFFRLR")>]    
    [<InlineData("BBFFBBFRLL")>]    
    let ``Boarding pass Id should be the sum of the row multiplied by 8 and column `` (seatLocation) =
        let boardingPass = Parser.parseBoardingPass seatLocation
        let product = boardingPass.row * 8 + boardingPass.column
        Assert.Equal(product, boardingPass.id)

    [<Theory>]
    [<InlineData("FFFBBBFRRR", 119)>]    
    [<InlineData("BFFFBBFRRR", 567)>]    
    [<InlineData("FBFBBFFRLR", 357)>]    
    [<InlineData("BBFFBBFRLL", 820)>]    
    let ``Boarding pass Id should be calculated correctly`` (seatLocation, expectedPassId) =
        let boardingPass = Parser.parseBoardingPass seatLocation
        Assert.Equal(expectedPassId, boardingPass.id)
    
    [<Fact>]
    let ``Should calculate boarding pass id 119 for FFFBBBFRRR`` () =
        let result = main "FFFBBBFRRR"
        Assert.Equal("119", result)
    
    [<Fact>]
    let ``Should calculate boarding pass id 357 for FBFBBFFRLR`` () =
        let result = main "FBFBBFFRLR"
        Assert.Equal("357", result)
    
    [<Fact>]
    let ``Should find the highest boarding pass ID for single elem`` () =
        let result = main "BFFFBBFRRR"
        Assert.Equal("567", result)
    
    [<Fact>]
    let ``Should find the highest boarding pass ID`` () =
        let result = main "FFFBBBFRRR
BFFFBBFRRR
FBFBBFFRLR
BBFFBBFRLL"
        Assert.Equal("820", result)