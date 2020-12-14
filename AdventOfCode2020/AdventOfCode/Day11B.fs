namespace AdventOfCode

open AdventOfCode.Day11A

module Day11B =
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
    
    let rec getValidSeatInDirection (translate : int*int -> int*int) (ferrySetup : Position[,]) (coord : int*int) =
        let height = Array2D.length1 ferrySetup
        let width = Array2D.length2 ferrySetup
        let (k,l) = translate coord

        if isValidCoordinate (k,l) height width then
            let neighbour = ferrySetup.[k,l]
            match neighbour with
                | Floor -> getValidSeatInDirection translate ferrySetup (k,l)
                | _ -> neighbour
        else
            Floor
    
    let getTopLeft ferrySetup (i,j) =
        getValidSeatInDirection (fun (i,j) -> (i-1, j-1)) ferrySetup (i,j)
    
    let getTop (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i - 1, j)) ferrySetup (i,j)

    let getTopRight (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i - 1, j + 1)) ferrySetup (i,j)
        
    let getLeft (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i, j - 1)) ferrySetup (i,j)
    
    let getRight (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i, j + 1)) ferrySetup (i,j)
    
    let getBottomRight (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i + 1, j + 1)) ferrySetup (i,j)
    
    let getBottomLeft (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i + 1, j - 1)) ferrySetup (i,j)
    
    let getBottom (ferrySetup : Position[,]) (i,j)  : Position =
        getValidSeatInDirection (fun (i,j) -> (i + 1, j)) ferrySetup (i,j)
    
    let getOccupiedNeighbourCount (ferrySetup : Position[,]) (i:int) (j:int) : int =
        let T = getTop ferrySetup (i, j)
        let TR = getTopRight ferrySetup (i, j)
        let L = getLeft ferrySetup (i, j)
        let R = getRight ferrySetup (i, j)
        let BL = getBottomLeft ferrySetup (i, j)
        let B = getBottom ferrySetup (i, j)
        let BR = getBottomRight ferrySetup (i ,j)
        let TL = getTopLeft ferrySetup (i, j)
         
        [TL; T; TR; L; R; BL; B; BR]
            |> Seq.filter (fun s -> s = Occupied)
            |> Seq.length
            
    let calculateSeat (ferrySetup : Position[,]) (i:int) (j:int) : Position =
        let seat = ferrySetup.[i,j]
        let occupiedNeighbourCount = getOccupiedNeighbourCount ferrySetup i j
        match seat with
            | Empty -> if occupiedNeighbourCount > 0 then Empty else Occupied
            | Occupied -> if occupiedNeighbourCount < 5 then Occupied else Empty
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
        
//        printfn "---"
//        for i in 0..(Array2D.length1 ferrySetup - 1) do
//            for j in 0..((Array2D.length2 ferrySetup) - 1) do
//                let a =
//                    match ferrySetup.[i,j] with
//                        | Floor -> "."
//                        | Empty -> "L"
//                        | Occupied -> "#"
//                printf "%s" a
//            printf "\r\n"
                
        
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