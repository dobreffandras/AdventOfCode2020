namespace AdventOfCode.Tests

open AdventOfCode.Day13A
open Xunit

module Day13A =
    
    [<Theory>]
    [<InlineData(939, 59, 5)>]
    [<InlineData(939, 7, 6)>]
    [<InlineData(939, 13, 10)>]
    [<InlineData(939, 19, 11)>]
    let ``calculates minutes of waiting until bus comes correctly`` arrival busFreq expectedMins =
        let minutes = minsWaitingForBusLine busFreq arrival
        Assert.Equal(expectedMins, minutes)
        
    [<Fact>]
    let ``selects best bus`` () =
        let bestBusAndWaitTime = selectBestBus 939 [59; 7; 13; 19]
        Assert.Equal((59, 5), bestBusAndWaitTime)
        
    [<Fact>]
    let ``runs correctly for sample input`` () =
        let answer = main AdventOfCode.Inputs.Day13.sample
        Assert.Equal("295", answer)
