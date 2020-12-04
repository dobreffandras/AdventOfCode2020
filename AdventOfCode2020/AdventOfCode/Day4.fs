namespace AdventOfCode

open System
open AdventOfCode

module Day4 =
    
    type FieldType =
            BirthYear | IssueYear | ExpirationYear | Height | HairColor | EyeColor | PassportID | CountryID
            
    type Field =
        {
            fieldType : FieldType
            // fieldContent : string -- no need for the puzzle
        }
    
    type Passport(fields : seq<Field>) =
            let requiredFields =
                [BirthYear;  IssueYear;  ExpirationYear;  Height;  HairColor;  EyeColor;  PassportID]
            
            let hasTypeOfField (fieldType : FieldType) : bool =
                Seq.exists (fun f -> f.fieldType = fieldType) fields
            
            member this.isValid : bool =
                Seq.forall hasTypeOfField requiredFields

    module Parser =
        let splitByBlankLine (input : string) : string [] =
            let sepString = Environment.NewLine + Environment.NewLine 
            input.Split([|sepString|], StringSplitOptions.RemoveEmptyEntries)
            
        let splitBySpaceOrNewline (input : string) : string [] =
            input.Split([|Environment.NewLine; " "|], StringSplitOptions.RemoveEmptyEntries)            
        
        let splitByColon (input : string) : string*string =
            let s = input.Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
            (s.[0], s.[1])
        
        let toField (field: string) : Field =
            let (rawFieldType, _) = splitByColon field
            let fieldType =
                match rawFieldType with
                    | "byr" -> BirthYear
                    | "iyr" -> IssueYear
                    | "eyr" -> ExpirationYear
                    | "hgt" -> Height
                    | "hcl" -> HairColor
                    | "ecl" -> EyeColor
                    | "pid" -> PassportID
                    | "cid" -> CountryID
                    | _ -> BirthYear
                
            { fieldType = fieldType }
        
        let toPassport (input : string []) : Passport =
            let fields = Seq.map toField input
            Passport(fields)
       
    let main =
        let rawPassports = Parser.splitByBlankLine Inputs.Day4.input
        let rawPassportsData : string [] [] =
            rawPassports
                |> Seq.map Parser.splitBySpaceOrNewline
                |> Seq.toArray
                
        let passports = Seq.map Parser.toPassport rawPassportsData
        
        passports
            |> Seq.filter (fun f -> f.isValid)
            |> Seq.length
            |> string
