namespace AdventOfCode

module Day5A =
    
    type Step = Up | Down
    
    type SeatLocation =
        {
            rowSteps : Step list
            columnSteps : Step list
        }
    
    type BoardingPass(seatLocation : SeatLocation) =
        
        let rec locate (lowerBound : int) (upperBound : int) (steps : Step list) : int =
            match steps with
                | step::tail ->
                    let middleValue = (lowerBound + upperBound) / 2
                    match step with
                        | Up ->
                            let newLowerBound =  middleValue + 1
                            locate newLowerBound upperBound tail
                        | Down ->
                            let newUpperBound = middleValue
                            locate lowerBound newUpperBound tail
                | [] -> lowerBound
        
        member this.id =
            this.row * 8 + this.column    
        
        member this.row : int =
             locate 0 127 seatLocation.rowSteps
        
        member this.column : int =
            locate 0 7 seatLocation.columnSteps
                
            
    module Parser =
        
        let parseRowSteps (input : string) : Step [] =
            let toStep (x: char) =
                match x with
                    | 'F' -> Down
                    | 'B' -> Up
                    | _ -> raise (System.ArgumentException("Unexpected row step."))
            Seq.toArray (Seq.map toStep input)

        let parseColumnSteps (input : string) : Step [] =
            let toStep (x: char) =
                match x with
                    | 'R' -> Up
                    | 'L' -> Down
                    | _ -> raise (System.ArgumentException("Unexpected row step."))
            Seq.toArray (Seq.map toStep input)
            
        
        let parseBoardingPass (input : string) : BoardingPass =
            let rawRowSteps = input.[0..6]
            let rawColumnSteps = input.[7..9]
            BoardingPass(
                {
                    rowSteps = parseRowSteps rawRowSteps |> Seq.toList
                    columnSteps = parseColumnSteps rawColumnSteps |> Seq.toList
                })
    
    
    let main input =
        let boardingPasses = Parser.parseToRows input
        boardingPasses
            |> Seq.map Parser.parseBoardingPass
            |> Seq.map (fun pass -> pass.id)
            |> Seq.max
            |> string
