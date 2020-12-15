namespace AdventOfCode.Tests

open AdventOfCode.Day12B
open Xunit

module Day12B =
    
    let initialState = { x=0; y=0; waypoint = { x = 10; y = 1 } } 
    
    [<Fact>]
    let ``The manhattan distance is 0 at start`` () =
        let state = runShip []
        Assert.Equal(0, state.distance)
    
    [<Fact>]
    let ``The waypoint at East 10 North 1 of the ship by default`` ()=
        let state = runShip []
        Assert.Equal({x = 10; y = 1} , state.waypoint)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The waypoint moves north for instruction North`` (d : int) =
        let state = runShip [North d]
        let w = initialState.waypoint
        let expectedWaypoint = {w with y = w.y + d }
        let expectedShipState = {initialState with waypoint = expectedWaypoint}
        Assert.Equal(expectedShipState, state)
        
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The waypoint moves south for instruction South`` (d : int) =
        let state = runShip [South d]
        let w = initialState.waypoint
        let expectedWaypoint = {w with y = w.y - d }
        let expectedShipState = {initialState with waypoint = expectedWaypoint}
        Assert.Equal(expectedShipState, state)
    
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The waypoint moves east for instruction East`` (d : int) =
        let state = runShip [East d]
        let w = initialState.waypoint
        let expectedWaypoint = {w with x = w.x + d }
        let expectedShipState = {initialState with waypoint = expectedWaypoint}
        Assert.Equal(expectedShipState, state)

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The waypoint moves west for instruction West`` (d : int) =
        let state = runShip [West d]
        let w = initialState.waypoint
        let expectedWaypoint = {w with x = w.x - d }
        let expectedShipState = {initialState with waypoint = expectedWaypoint}
        Assert.Equal(expectedShipState, state)
        
    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(30)>]
    let ``The ship moves by the waypoint vector X times for instruction Forward`` (d : int) =
        let state = runShip [Forward d]
        let w = initialState.waypoint
        let newX = initialState.x + w.x * d
        let newY = initialState.y + w.y * d
        let expectedShipState = {initialState with x = newX; y = newY}
        Assert.Equal(expectedShipState, state)
        
    [<Theory>]
    [<InlineData(12, 3, -3, 12)>]
    [<InlineData(-3, 12, -12, -3)>]
    [<InlineData(-12, -3, 3, -12)>]
    [<InlineData(3, -12, 12, 3)>]
    let ``runInstruction - The waypoint turns 90° left for instruction Left90`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Left D90)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)
        
    [<Theory>]
    [<InlineData(-3, 12, 12, 3 )>]
    [<InlineData(12, 3, 3, -12)>]
    [<InlineData(3, -12, -12, -3)>]
    [<InlineData(-12, -3, -3, 12)>]
    let ``runInstruction - The waypoint turns 90° right for instruction Right90`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Right D90)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)
          
    [<Theory>]
    [<InlineData(-3, 12, 3, -12 )>]
    [<InlineData(-12, -3, 12, 3)>]
    [<InlineData(3, -12, -3, 12)>]
    [<InlineData(12, 3, -12, -3)>]
    let ``The waypoint turns around for instruction Left 180`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Left D180)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)
    
    [<Theory>]
    [<InlineData(-3, 12, 3, -12 )>]
    [<InlineData(-12, -3, 12, 3)>]
    [<InlineData(3, -12, -3, 12)>]
    [<InlineData(12, 3, -12, -3)>]
    let ``The waypoint turns around for instruction Right 180`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Right D180)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)
          
      
    [<Theory>]
    [<InlineData(-3, 12, 12, 3 )>]
    [<InlineData(12, 3, 3, -12)>]
    [<InlineData(3, -12, -12, -3)>]
    [<InlineData(-12, -3, -3, 12)>]
    let ``runInstruction - The waypoint turns 270° right for instruction Left270`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Left D270)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)


    [<Theory>]
    [<InlineData(-3, 12, 12, 3 )>]
    [<InlineData(12, 3, 3, -12)>]
    [<InlineData(3, -12, -12, -3)>]
    [<InlineData(-12, -3, -3, 12)>]
    let ``runInstruction - The waypoint turns 270° left for instruction Left270`` x y newX newY =
        let initialState = { x = 10; y = 31; waypoint = { x = x; y = y } }
        let state = runInstruction initialState (Left D270)
        let expectedState = { x = 10; y = 31; waypoint = { x = newX; y = newY } }
        Assert.Equal(expectedState, state)

    [<Fact>]
    let ``The manhattan distance after moving F15 R90 F25`` () =
        (*
                (   0,    0, wp:  10,   1)
            F15 ( 150,   15, wp:  10,   1)
            R90 ( 150,   15, wp:   1, -10)
            F25 ( 175, -235, wp:   1, -10)
            --------------------------
            Distance: 175+235 = 410
        *)
        let state = runShip [Forward 15; Right D90; Forward 25]
        Assert.Equal(410, state.distance)
        
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
        
        Assert.Equal("286", result)
        
    [<Fact>]
    let ``Runs for puzzle input correctly`` () =
        let result = main AdventOfCode.Inputs.Day12.input
        Assert.Equal("156735", result)
        
        
    
            
        

