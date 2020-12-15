namespace AdventOfCode

module Day12A =
    
    type Orientation = North | East | South | West
    
    type Degree = D90 | D180 | D270
    
    type Instruction =
        | North of int 
        | South of int 
        | East of int 
        | West of int 
        | Forward of int
        | Left of Degree 
        | Right of Degree 
    
    type ShipState =
        { x: int; y: int; direction: Orientation}
            with
            member this.distance = (abs this.x) + (abs this.y)
    
    module Parser =
        let toDegree (number : int) : Degree =
            match number with
                | 90 -> D90
                | 180 -> D180
                | 270 -> D270
                | _ -> raise (System.ArgumentException("Unexpected degree "))
            
        let parseInstruction (instRow : string) : Instruction =
            let i = instRow.[0]
            let rawNumber = instRow.[1..]
            let number = int rawNumber
            match i with
                | 'N' -> North number
                | 'E' -> East number
                | 'S' -> South number
                | 'W' -> West number
                | 'F' -> Forward number
                | 'L' -> Left (toDegree number)
                | 'R' -> Right (toDegree number)
                | _ -> raise (System.ArgumentException("Unexpected instruction"))
    
    let turnLeft shipState =
        let newDirection : Orientation=
            match shipState.direction with
                | Orientation.North -> Orientation.West
                | Orientation.West -> Orientation.South
                | Orientation.South -> Orientation.East
                | Orientation.East -> Orientation.North
        {shipState with direction = newDirection}
    
    let turnAround (shipState : ShipState) : ShipState =
         shipState |> (turnLeft >> turnLeft)
    
    let turnRight shipState =
        let newDirection : Orientation=
            match shipState.direction with
                | Orientation.North -> Orientation.East
                | Orientation.South -> Orientation.West
                | Orientation.West -> Orientation.North
                | Orientation.East -> Orientation.South
        {shipState with direction = newDirection}
    
    let turnShipLeft degree shipState =
        match degree with
            | D90 -> turnLeft shipState
            | D180 -> turnAround shipState
            | D270 -> turnRight shipState
    
    let turnShipRight degree shipState =
        match degree with
            | D90 -> turnRight shipState
            | D180 -> turnAround shipState
            | D270 -> turnLeft shipState
    
    let convertForwardToInstruction state d =
        match state.direction with
            | Orientation.North -> North d
            | Orientation.East -> East d
            | Orientation.South -> South d
            | Orientation.West -> West d
    
    let rec runInstruction state instruction =
        match instruction with
            | North d -> {state with y = state.y + d}
            | South d -> {state with y = state.y - d}
            | East d -> {state with x = state.x + d}
            | West d -> {state with x = state.x - d}
            | Forward d -> runInstruction state (convertForwardToInstruction state d)
            | Left d -> turnShipLeft d state 
            | Right d -> turnShipRight d state 
    
    let runShip (instructions : Instruction list) : ShipState = 
        instructions |> Seq.fold runInstruction { x=0; y=0; direction = Orientation.East} 

    let main (input : string) =
        let instructions = 
            input
                |> Parser.parseToRows
                |> Seq.map Parser.parseInstruction
                |> Seq.toList
        
        (runShip instructions).distance |> string

