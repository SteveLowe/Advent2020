module advent2020.day4

open System
open System.IO
open System.Text.RegularExpressions

type Passport =
    { BirthYear: string
      IssueYear: string
      ExpirationYear: string
      Height: string
      HairColor: string
      EyeColor: string
      PassportID: string
      CountryID: string }

type PendingPassports =
    { Passports: Passport list
      Current: Passport }
    member this.AllPassports() =
        (this.Current :: this.Passports) |> List.rev

let Solve () =
    let blankPassport =
        { BirthYear = ""
          IssueYear = ""
          ExpirationYear = ""
          Height = ""
          HairColor = ""
          EyeColor = ""
          PassportID = ""
          CountryID = "" }

    let updatePassport field value passport =
        match field with
        | "byr" -> { passport with BirthYear = value }
        | "iyr" -> { passport with IssueYear = value }
        | "eyr" -> { passport with ExpirationYear = value }
        | "hgt" -> { passport with Height = value }
        | "hcl" -> { passport with HairColor = value }
        | "ecl" -> { passport with EyeColor = value }
        | "pid" -> { passport with PassportID = value }
        | "cid" -> { passport with CountryID = value }
        | _ -> failwithf "%s is not a valid passport field" field

    let parsePassportField passport (keyValue: string) =
        let kv = keyValue.Split(':', 2)

        match kv.Length with
        | 2 -> passport |> updatePassport kv.[0] kv.[1]
        | _ -> passport // invalid input

    let parsePassportLine (pendingPassport: PendingPassports) line =
        if String.IsNullOrWhiteSpace line then
            { Passports = (pendingPassport.AllPassports())
              Current = blankPassport }
        else
            let passport =
                line.Split()
                |> Array.fold parsePassportField pendingPassport.Current

            { pendingPassport with
                  Current = passport }

    let passportFieldsPresent (passport: Passport) =
        passport.BirthYear <> ""
        && passport.IssueYear <> ""
        && passport.ExpirationYear <> ""
        && passport.Height <> ""
        && passport.HairColor <> ""
        && passport.EyeColor <> ""
        && passport.PassportID <> ""

    let isIntRange min max value =
        try
            let num = int value
            num >= min && num <= max
        with :? FormatException -> false

    let isValidHeight value =
        let parseHeight (value: String) =
            if value.Length > 3
            then (value.[..(value.Length - 3)], value.[(value.Length - 2)..])
            else (value, "")

        let (num, unit) = parseHeight value

        match unit with
        | "cm" -> isIntRange 150 193 num
        | "in" -> isIntRange 59 76 num
        | _ -> false

    let isValidHairColor value =
        Regex.Match(value, "^\#[0-9a-f]{6}$").Success

    let isValidEyeColor value =
        match value with
        | "amb"
        | "blu"
        | "brn"
        | "gry"
        | "grn"
        | "hzl"
        | "oth" -> true
        | _ -> false

    let isValidPassportId value =
        Regex.Match(value, "^[0-9]{9}$").Success

    let passportValid (passport: Passport) =
        (passport.BirthYear |> isIntRange 1920 2002)
        && (passport.IssueYear |> isIntRange 2010 2020)
        && (passport.ExpirationYear |> isIntRange 2020 2030)
        && (passport.Height |> isValidHeight)
        && (passport.HairColor |> isValidHairColor)
        && (passport.EyeColor |> isValidEyeColor)
        && (passport.PassportID |> isValidPassportId)

    let pendingPassports =
        File.ReadAllLines "inputs/day4.txt"
        |> Array.fold
            parsePassportLine
               { Passports = []
                 Current = blankPassport }

    let fieldsPresentPassports =
        pendingPassports.AllPassports()
        |> List.filter passportFieldsPresent

    let validPassports =
        pendingPassports.AllPassports()
        |> List.filter passportValid

    let answer1 = fieldsPresentPassports.Length
    printfn "day4-part1:\n  Passports: %i\n  Answer: %i" (pendingPassports.AllPassports().Length) answer1

    let answer2 = validPassports.Length
    printfn "day4-part2:\n  Answer: %i" answer2
