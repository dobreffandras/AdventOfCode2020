namespace AdventOfCode.Tests

open AdventOfCode.Day12A
open Xunit

module Day12A =
    
    let initialState = { x=0; y=0; direction = Orientation.East } 
    
    [<Fact>]
    let ``The manhattan distance is 0 at start`` () =
        let state = runShip []
        Assert.Equal(0, state.distance)
    
    [<Fact>]
    let ``The ship faces East by default`` ()=
        let state = runShip []
        Assert.Equal(Orientation.East , state.direction)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves north for instruction North`` (d : int) =
        let state = runShip [North d]
        Assert.Equal({initialState with y = d}, state)
        
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves south for instruction South`` (d : int) =
        let state = runShip [South d]
        Assert.Equal({initialState with y = -d}, state)
    
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves east for instruction East`` (d : int) =
        let state = runShip [East d]
        Assert.Equal({initialState with x = d}, state)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves west for instruction West`` (d : int) =
        let state = runShip [West d]
        Assert.Equal({initialState with x = -d}, state)
        
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves east for instruction Forward`` (d : int) =
        let state = runShip [Forward d]
        Assert.Equal({initialState with x = d}, state)
        
    [<Fact>]
    let ``The ship turns east->north for instruction Left90`` () =
        let state = runShip [Left D90]
        Assert.Equal({initialState with direction = Orientation.North}, state)
        
    [<Fact>]
    let ``The ship turns east->south for instruction Right90`` () =
        let state = runShip [Right D90]
        Assert.Equal({initialState with direction = Orientation.South}, state)
        
    [<Fact>]
    let ``The ship turns east->west for instruction Left90 Left90`` () =
        let state = runShip [Left D90; Left D90]
        Assert.Equal({initialState with direction = Orientation.West}, state)
        
        
    [<Fact>]
    let ``The ship turns east->west for instruction Right90 Right90`` () =
        let state = runShip [Right D90; Right D90]
        Assert.Equal({initialState with direction = Orientation.West}, state)
        
    [<Fact>]
    let ``The ship turns east->west for instruction Left 180`` () =
        let state = runShip [Left D180]
        Assert.Equal({initialState with direction = Orientation.West}, state)
        
    [<Fact>]
    let ``The ship turns east->west for instruction Right 180`` () =
        let state = runShip [Right D180]
        Assert.Equal({initialState with direction = Orientation.West}, state)
        
          
    [<Fact>]
    let ``The ship moves north for instructions Left90 and Forward`` () =
        let state = runShip [Left D90; Forward 10]
        let expectedShipState = 
            {initialState
             with
                 direction = Orientation.North
                 y = 10}
        Assert.Equal(expectedShipState, state)
        
    [<Fact>]
    let ``The manhattan distance after moving south 25 and east 15`` () =
        let state = runShip [Forward 15; Right D90; Forward 25]
        Assert.Equal(40, state.distance)
        
    [<Fact>]
    let ``Parses instructions correctly`` () =
         let result =
            ["N10"; "E11"; "S2"; "W1"; "F9";"L90"; "L180"
             "L270"; "R90"; "R180"; "R270"]
                |> List.map Parser.parseInstruction
         
         let expected =
             [North 10; East 11; South 2; West 1; Forward 9;
              Left D90; Left D180; Left D270; Right D90; Right D180; Right D270]
         
         Assert.Equal<Instruction list>(expected, result)
        
    [<Fact>]
    let ``Runs for sample correctly`` () =
        let result = main AdventOfCode.Inputs.Day12.sample
        
        Assert.Equal("25", result)
        
    [<Fact>]
    let ``Runs for puzzle input correctly`` () =
        let result = main AdventOfCode.Inputs.Day12.input
        
        Assert.Equal("1032", result)
        
        
    
            
        

