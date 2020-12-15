namespace AdventOfCode

module Day12B =
    
    type Orientation = North | East | South | West
    
    type Degree = D90 | D180 | D270
    
    type WayPoint = { x: int; y: int }
    
    type Instruction =
        | North of int 
        | South of int 
        | East of int 
        | West of int 
        | Forward of int
        | Left of Degree 
        | Right of Degree 
    
    type ShipState =
        { x: int; y: int; waypoint: WayPoint}
            with
            member this.distance = (abs this.x) + (abs this.y)
            
            member this.updateWayPoint (updater: WayPoint -> WayPoint) =
                let newWp = updater this.waypoint
                {this with waypoint = newWp}
    
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

    let moveWayPointNorth (shipState : ShipState) d =
        shipState.updateWayPoint (fun wp -> {wp with y = wp.y + d})

    let moveWayPointSouth (shipState : ShipState) d =
        shipState.updateWayPoint (fun wp -> {wp with y = wp.y - d})
    
    let moveWayPointEast (shipState : ShipState) d =
        shipState.updateWayPoint (fun wp -> {wp with x = wp.x + d})
        
    let moveWayPointWest (shipState : ShipState) d =
        shipState.updateWayPoint (fun wp -> {wp with x = wp.x - d})
    
    let turnLeft (shipState : ShipState) =
        shipState.updateWayPoint (fun wp -> { x= -wp.y; y= wp.x })
    
    let turnAround (shipState : ShipState) = 
        shipState.updateWayPoint (fun wp -> { x= -wp.x; y= -wp.y })
    
    let turnRight (shipState : ShipState) =
        shipState.updateWayPoint (fun wp -> { x= wp.y; y= -wp.x })
    
    
    let moveShip shipState d =
        let wp = shipState.waypoint
        let newX = shipState.x + wp.x * d
        let newY = shipState.y + wp.y * d
        { x = newX; y = newY; waypoint = wp }

    let turnWayPointLeft shipState degree =
        match degree with
            | D90 -> turnLeft shipState
            | D180 -> turnAround shipState
            | D270 -> turnRight shipState

    let turnWayPointRight shipState degree =
        match degree with
            | D90 -> turnRight shipState
            | D180 -> turnAround shipState
            | D270 -> turnLeft shipState
    
    let rec runInstruction state instruction =
        match instruction with
            | North d -> moveWayPointNorth state d
            | South d -> moveWayPointSouth state d
            | East d -> moveWayPointEast state d
            | West d -> moveWayPointWest state d
            | Forward d -> moveShip state d
            | Left d -> turnWayPointLeft state d
            | Right d -> turnWayPointRight state d
    
    let runShip (instructions : seq<Instruction>) : ShipState = 
        let initialShipState = { x = 0; y = 0; waypoint = {x = 10; y = 1} }
        instructions |> Seq.fold runInstruction initialShipState 
        
        
    let main (input : string) =
        let instructions = 
            input
                |> Parser.parseToRows
                |> Seq.map Parser.parseInstruction
        
        (runShip instructions).distance |> string

