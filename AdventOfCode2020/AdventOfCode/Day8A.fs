namespace AdventOfCode

open System.Management.Instrumentation

module Day8A =

    type Instruction =
        | NoOp
        | Acc of int
        | Jump of int
        
    
    type Program =
        {
            instructions :  Instruction [];
            alreadyRunIndexes : int list;
            accumulator : int;
            instructionIndex : int;
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
        
    let rec runUntilInfiniteLoop (program : Program) : int =
        let terminates =
            program.instructionIndex >= program.instructions.Length
        let infiniteLoopDetected =
            List.contains program.instructionIndex program.alreadyRunIndexes
        if terminates || infiniteLoopDetected then
            program.accumulator
        else
            let nextProgram = program.calculateNextProgram()
            runUntilInfiniteLoop nextProgram

    module Parser =
        
        let parseInstruction (inst: string) (param : string) : Instruction =
            let p = int param
            match inst with
                | "acc" -> Acc p
                | "jmp" -> Jump p
                | _ -> NoOp
        
        let parseInstructionLine (input : string) : Instruction =
            let rawInstructionWithParam =
                Parser.splitBySpace input
            let rawInstruction = rawInstructionWithParam.[0]
            let rawParam = rawInstructionWithParam.[1]
            parseInstruction rawInstruction rawParam

    let main (input : string) : int =
        let instructions =
            Parser.parseToRows input
                |> Seq.map Parser.parseInstructionLine
                |> Seq.toArray
        
        runUntilInfiniteLoop {
            instructions = instructions;
            accumulator = 0
            alreadyRunIndexes = [];
            instructionIndex = 0;
        }
        

