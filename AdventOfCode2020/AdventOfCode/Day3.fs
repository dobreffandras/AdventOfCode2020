namespace AdventOfCode

open AdventOfCode.Day2

module Day3 =
    
    type Slope = { right : int; down : int }
    type Coordinate = Tree | OpenSquare
    
    type Hill(rows : Coordinate list list) =
        member this.height =
            rows.Length
            
        member this.width =
            rows.[0].Length

        member this.coordinateOnPosition (x, y) : Coordinate =
            rows.[y].[ x % this.width ]
        
    module Parser =
        let charToCoordinate(c : char) : Coordinate =
            match c with
                | '#' -> Tree
                | _ -> OpenSquare
          
        let parseRow(row : string) : Coordinate list =
            Seq.toList (Seq.map charToCoordinate row)
    
    let rec travel ((x, y) : int*int)  (hill :Hill) (slope : Slope) : Coordinate list=
        if y >= hill.height then
            []
        else
            let coordinate = hill.coordinateOnPosition (x,y)
            let newPos = (x + slope.right, y + slope.down)
            coordinate :: (travel newPos hill slope)
        
    
    let toTrajectory slope hill =
        let position = (0, 0)
        travel position hill slope
    
    let countTreesInTrajectory trajectory : int64 =
        trajectory
            |> Seq.filter (fun c -> c = Tree)
            |> Seq.length
            |> int64
    
    let main =
        let rows = AdventOfCode.Parser.parseToRows Inputs.Day3.input
        let parsedRows = Seq.map Parser.parseRow rows
        let hill = Hill(Seq.toList parsedRows)
        let trajectory1 = toTrajectory {right = 1; down = 1} hill
        let trajectory2 = toTrajectory {right = 3; down = 1} hill
        let trajectory3 = toTrajectory {right = 5; down = 1} hill
        let trajectory4 = toTrajectory {right = 7; down = 1} hill
        let trajectory5 = toTrajectory {right = 1; down = 2} hill
        let t1 = countTreesInTrajectory trajectory1
        let t2 = countTreesInTrajectory trajectory2
        let t3 = countTreesInTrajectory trajectory3
        let t4 = countTreesInTrajectory trajectory4
        let t5 = countTreesInTrajectory trajectory5
        let multiplied = (t1*t2*t3*t4*t5)
        printfn "%d*%d*%d*%d*%d = %d" t1 t2 t3 t4 t5 multiplied
        string multiplied
        
            

