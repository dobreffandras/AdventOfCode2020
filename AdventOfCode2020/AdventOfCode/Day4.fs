namespace AdventOfCode

open System
open AdventOfCode
open System.Text.RegularExpressions

module Day4 =
    
    type FieldType =
        | BirthYear
        | IssueYear
        | ExpirationYear
        | Height
        | HairColor
        | EyeColor
        | PassportID
        | CountryID
        with
        
            member private this.isValidBirthYear (content: string) : bool =
                let (isParseSuccessful, parseResult) =  Int32.TryParse(content)
                isParseSuccessful && (1920 <= parseResult) && (parseResult <= 2002)
                
            member private this.isValidIssueYear (content: string) : bool =
                let (isParseSuccessful, parseResult) =  Int32.TryParse(content)
                isParseSuccessful && (2010 <= parseResult) && (parseResult <= 2020)

            member private this.isValidExpirationYear (content: string) : bool =
                let (isParseSuccessful, parseResult) =  Int32.TryParse(content)
                isParseSuccessful && (2020 <= parseResult) && (parseResult <= 2030)
                
            member private this.isValidHeight (content: string) : bool =
                let hasValidFormat = Regex.IsMatch(content, "[0-9]*(cm|in)")                
                let isCentimeter = Regex.IsMatch(content, "cm")
                let height : int = (Regex.Split(content, "cm|in").[0]) |> int
                let isInValidRange =
                    if (isCentimeter) then
                        (150 <= height && height <= 193)
                    else
                        (59 <= height && height <= 76)
                hasValidFormat && isInValidRange

            member private this.isValidHairColor (content: string) : bool =
                Regex.IsMatch(content, "#{1}[a-f0-9]{6}")     

            member private this.isValidEyeColor (content: string) : bool =
                Regex.IsMatch(content, "amb|blu|brn|gry|grn|hzl|oth")

            member private this.isValidPassportID (content: string) : bool =
                let hasValidLength = (content.Length = 9)
                let isNumber = (Int32.TryParse(content) |> fst)
                hasValidLength && isNumber
            
            member this.isValid (content : string) : bool =
                match this with
                    | BirthYear -> this.isValidBirthYear content
                    | IssueYear -> this.isValidIssueYear content
                    | ExpirationYear -> this.isValidExpirationYear content
                    | Height -> this.isValidHeight content
                    | HairColor -> this.isValidHairColor content
                    | EyeColor -> this.isValidEyeColor content
                    | PassportID -> this.isValidPassportID content
                    | CountryID -> true
            
    type Field =
        {
            fieldType : FieldType
            fieldContent : string
        } with
            member this.isValid : bool =
                this.fieldType.isValid this.fieldContent
    
    type Passport(fields : seq<Field>) =
            let requiredFields =
                [BirthYear;  IssueYear;  ExpirationYear;  Height;  HairColor;  EyeColor;  PassportID]
            
            let hasTypeOfField (fieldType : FieldType) : bool =
                Seq.exists (fun f -> f.fieldType = fieldType) fields
            
            member this.isValid : bool =
                (Seq.forall hasTypeOfField requiredFields)
                && (Seq.forall (fun (f : Field) -> f.isValid) fields)

    module Parser =
            
        let splitBySpaceOrNewline (input : string) : string [] =
            input.Split([|Environment.NewLine; " "|], StringSplitOptions.RemoveEmptyEntries)            
        
        let splitByColon (input : string) : string*string =
            let s = input.Split([|":"|], StringSplitOptions.RemoveEmptyEntries)
            (s.[0], s.[1])
        
        let toField (field: string) : Field =
            let (rawFieldType, fieldContent) = splitByColon field
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
                
            { fieldType = fieldType; fieldContent = fieldContent }
        
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
