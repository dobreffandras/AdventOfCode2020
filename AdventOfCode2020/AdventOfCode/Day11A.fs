namespace AdventOfCode

module Day11A =
    type Position = Floor | Empty | Occupied
    
    module Parser = 
        
        let parsePosition (p : char) : Position =
            match p with
                | 'L' -> Empty
                | '#' -> Occupied
                | _ -> Floor
        
        let parseRow (input : string) : Position list =
            input |> Seq.map parsePosition |> Seq.toList
        
        let parseFerrySeats (input : string) : Position [,] =
            Parser.parseToRows input
                |> Seq.map parseRow
                |> Seq.toList
                |> array2D
    
    let isValidCoordinate ((i,j) : int*int) (height:int) (width:int) : bool =
        0 <= i && i < height
        &&
        0 <= j && j < width
    
    let getOccupiedNeighbourCount (ferrySetup : Position[,]) (i:int) (j:int) : int =
        let TL = (i - 1, j - 1)
        let T = (i - 1, j)
        let TR = (i - 1, j + 1)
        let L = (i, j - 1)
        let R = (i, j + 1)
        let BL = (i + 1, j - 1)
        let B = (i + 1, j)
        let BR = (i + 1, j + 1)
        let height = Array2D.length1 ferrySetup
        let width = Array2D.length2 ferrySetup

        let validCoords = 
            [TL; T; TR; L; R; BL; B; BR]
                |> Seq.filter (fun c -> isValidCoordinate c height width)
                |> Seq.toArray
        validCoords
            |> Seq.map (fun (i,j) -> ferrySetup.[i,j])
            |> Seq.filter (fun s -> s = Occupied)
            |> Seq.length
            
    let calculateSeat (ferrySetup : Position[,]) (i:int) (j:int) : Position =
        let seat = ferrySetup.[i,j]
        let occupiedNeighbourCount = getOccupiedNeighbourCount ferrySetup i j
        match seat with
            | Empty -> if occupiedNeighbourCount > 0 then Empty else Occupied
            | Occupied -> if occupiedNeighbourCount < 4 then Occupied else Empty
            | Floor -> Floor
        
    let nextFerrySetup (ferrySetup : Position [,]) : Position [,] =
        let height = Array2D.length1 ferrySetup
        let width = Array2D.length2 ferrySetup
        let nextFerrySetup = Array2D.create height width Floor
        for i in 0..(height-1) do
            for j in 0..(width-1) do
                if ferrySetup.[i,j] <> Floor then
                    nextFerrySetup.[i,j] <- calculateSeat ferrySetup i j
        nextFerrySetup
    
    let rec occupySeats (ferrySetup : Position [,]) : Position [,] =
        let nextSetup = nextFerrySetup ferrySetup
        if ferrySetup = nextSetup then
            nextSetup
        else
            occupySeats nextSetup
            
    let main (input : string) : string =
        let occupiedSeats = 
            Parser.parseFerrySeats input
                |> occupySeats
        
        occupiedSeats
            |> Seq.cast<Position> // used to create a flattened variant
            |> Seq.filter (fun s -> s = Occupied)
            |> Seq.length
            |> string