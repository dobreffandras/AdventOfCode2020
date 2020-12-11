namespace AdventOfCode.Tests

open AdventOfCode
open Xunit

module Day9A =

    [<Fact>]
    let ``returns the invalid third number with preamble 2`` () =
        let result = Day9A.findFirstInvalidNumber 2 [1L; 2L; 5L]
        Assert.Equal(5L, result)
    
    [<Fact>]
    let ``doesn't return valid number with preamble 2`` () =
        let result = Day9A.findFirstInvalidNumber 2 [1L;2L;3L;6L]
        Assert.Equal(6L, result)

    [<Fact>]
    let ``returns invalid number with preamble 5`` () =
        let result = Day9A.findFirstInvalidNumber 5 [65L; 95L; 102L; 117L; 150L; 182L; 127L]
        Assert.Equal(127L, result)
        
    [<Fact>]
    let ``returns 127 for sampleA preamble 5`` () =
        let result = Day9A.findFirstInvalidNumber 5 [35L;20L;15L;25L;47L;40L;62L;55L;65L;95L;102L;117L;150L;182L;127L;219L;299L;277L;309L;576L]
        Assert.Equal(127L, result)
        

