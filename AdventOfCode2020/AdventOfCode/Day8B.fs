namespace AdventOfCode

open AdventOfCode.Day8A

module Day8B =

    [<DefaultAugmentation(false)>] // Don't generate default Is* functions 
    type Instruction =
        | NoOp of int
        | Acc of int
        | Jump of int
        with
            member this.IsNoOp =
                match this with | NoOp _ -> true | _ -> false
            member this.IsJump =
                match this with | Jump _ -> true | _ -> false
            member this.IsAcc =
                match this with | Acc _ -> true | _ -> false
            
        
        
    
    type Program =
        {
            instructions :  Instruction [];
            alreadyRunIndexes : int list;
            accumulator : int;
            instructionIndex : int;
            terminatedSuccessfully : Option<bool>;
        } with
      
        member this.nextInstructionIndex : int =
            let inst = this.instructions.[this.instructionIndex]
            match inst with
                | Jump jump -> this.instructionIndex + jump
                | _ -> this.instructionIndex + 1
        
        member this.calculateNextAccumulator : int =
            let inst = this.instructions.[this.instructionIndex]
            match inst with
                | Acc increment -> this.accumulator + increment
                | _ -> this.accumulator
            
        member this.calculateNextProgram() : Program =
            let i = this.nextInstructionIndex
            let acc = this.calculateNextAccumulator
            let runInstructionIndexes =
                this.instructionIndex :: this.alreadyRunIndexes
            {
                this with
                    accumulator = acc
                    instructionIndex = i
                    alreadyRunIndexes = runInstructionIndexes
            } 
        
    let rec run(program : Program) : Program =
        let terminates =
            program.instructionIndex >= program.instructions.Length
        let infiniteLoopDetected =
            List.contains program.instructionIndex program.alreadyRunIndexes
        if terminates then
            { program with terminatedSuccessfully = Some true }
        elif infiniteLoopDetected then
            { program with terminatedSuccessfully = Some false }
        else
            let nextProgram = program.calculateNextProgram()
            run nextProgram

    module Parser =
        
        let parseInstruction (inst: string) (param : string) : Instruction =
            let p = int param
            match inst with
                | "acc" -> Acc p
                | "jmp" -> Jump p
                | _ -> NoOp p
        
        let parseInstructionLine (input : string) : Instruction =
            let rawInstructionWithParam =
                Parser.splitBySpace input
            let rawInstruction = rawInstructionWithParam.[0]
            let rawParam = rawInstructionWithParam.[1]
            parseInstruction rawInstruction rawParam

    let flipInstructionAtIndex (instructions : Instruction []) (idx : int) : Instruction [] =
        let flip (x: Instruction) : Instruction =
            match x with
                | NoOp p -> Jump p
                | Jump p -> NoOp p
                | a -> a
        
        Array.mapi (fun i x -> if i = idx then flip x else x) instructions
    
    let main (input : string) : int =
        let instructions =
            Parser.parseToRows input
                |> Seq.map Parser.parseInstructionLine
                |> Seq.toArray
        
        let indexesOfNoOpAndJump =
            instructions
                |> Array.indexed
                |> Seq.filter (fun (_, e) -> e.IsJump || e.IsNoOp)
                |> Seq.map (fun (i, _) -> i)
                
        let programVariants =
            indexesOfNoOpAndJump
                |> Seq.map (fun i ->  flipInstructionAtIndex instructions i)
                |> Seq.map (fun alteredInstructions ->
                    {
                        instructions = alteredInstructions;
                        accumulator = 0
                        alreadyRunIndexes = [];
                        instructionIndex = 0
                        terminatedSuccessfully = None
                    })
        
        let program =
            programVariants
                |> Seq.map run
                |> Seq.find (fun p -> p.terminatedSuccessfully = Some true)

        
        program.accumulator
        

