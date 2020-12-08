namespace AdventOfCode

open System


module Day2 =
    
    type PasswordPolicy =
        {
            character : char
            positionA: int
            positionB: int
        }
    type Entry =
        {
            policy : PasswordPolicy
            password : string
        }
    
    module Parser =
        let toEntry (splitRow : string []) : Entry = 
            let minMaxSplit = (splitRow.[0]).Split([|"-"|], StringSplitOptions.RemoveEmptyEntries)
            let minCount = int (minMaxSplit.[0])
            let maxCount = int (minMaxSplit.[1])
            let character = splitRow.[1].[0]
            let password = splitRow.[2]
            {
                policy =
                    {
                        positionA = minCount
                        positionB = maxCount
                        character = character
                    }
                password = password
            }
            
        let parseEntry : string -> Entry = (Parser.splitBySpace >> toEntry)
    
    let isValidPassword(entry : Entry) : bool =
        let policy = entry.policy
        let charA =  Seq.item (policy.positionA-1) entry.password 
        let charB =  Seq.item (policy.positionB-1) entry.password
        let matchesA = charA = policy.character
        let matchesB = charB = policy.character
        (matchesA || matchesB) && (not (matchesA && matchesB))
    
    let main : string =
        let rows = Inputs.Day2.input.Split([|Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        let entries = Seq.map Parser.parseEntry rows
        
        entries
            |> Seq.map (fun entry -> isValidPassword entry)
            |> Seq.filter id
            |> Seq.length
            |> string