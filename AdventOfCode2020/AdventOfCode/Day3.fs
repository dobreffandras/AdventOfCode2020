namespace AdventOfCode

open AdventOfCode.Day2

module Day3 =
    
    type Coordinate = Tree | OpenSquare
    
    type Row(row : Coordinate list) =
        member this.nthCoordinate( n : int) : Coordinate =
            row.[(n % row.Length)]
    
    type Hill(rows : Row list) =
        let rec reduceRows (nextCoordinateIndex : int) (rows : Row list) : Coordinate list =
            match rows with
                | [] -> []
                | (head::tail) -> 
                    let coordinate = (head.nthCoordinate nextCoordinateIndex)
                    coordinate :: (reduceRows (nextCoordinateIndex + 3) tail)
        
        member this.reduceRowsToCoordinatesOnSlope =
            reduceRows 0 rows 
            
    module Parser =
        
        let charToCoordinate(c : char) : Coordinate =
            match c with
                | '#' -> Tree
                | _ -> OpenSquare
          
        let parseRow(row : string) : Row =
            let coordinates =  Seq.toList (Seq.map charToCoordinate row)
            Row(coordinates)
            
    let main =
        let rows = AdventOfCode.Parser.parseToRows Inputs.Day3.input
        let parsedRows = Seq.map Parser.parseRow rows
        let hill = Hill(Seq.toList parsedRows)
        hill.reduceRowsToCoordinatesOnSlope
            |> Seq.filter (fun c -> c = Tree)
            |> Seq.length
            |> string
            

