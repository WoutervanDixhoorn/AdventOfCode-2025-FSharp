#load "../Common/utils.fsx"

#r "nuget: FParsec"
open FParsec

open Utils
open System.Collections.Generic

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

type Machine = {
    Lights: int list;
    Buttons: int list list;
    JReq: int list;
}

let pLightBit = (pchar '#' >>% 1) <|> (pchar '.' >>% 0)
let pLights = between (pstring "[") (pstring "]") (many pLightBit)

let pButton str = 
    (between (pstring "(") (pstring ")") (sepBy pint32 (pstring "," .>> spaces))
    .>> spaces) str
let pButtons str = (many pButton) str

let pJoltageLevel str = 
    (between (pstring "{") (pstring "}") (sepBy pint32 (pstring "," .>> spaces)) 
    .>> spaces) str
let pJoltageLevels str =  (many pJoltageLevel) str

let machines =
    input |>
    List.map (fun line ->
        let machineParser = 
            pipe3 
                (pLights .>> spaces) 
                (pButtons .>> spaces) 
                pJoltageLevels
                (fun l b j -> (l, b, j))

        match run machineParser line with
        | Success((l, b, j), _, _) -> (l, b, j)
        | Failure(err, _, _) -> failwith "Unreachable"
    ) |>
    List.map (fun (lights, buttons, joltage) -> 
        let buttonEffects =
            buttons |> List.map (fun indices -> 
                List.init lights.Length (fun i -> 
                    if List.contains i indices then 1 else 0
                )
            )
        {
            Lights = lights;
            Buttons = buttonEffects;
            JReq = List.concat joltage;
        }
    )

//Calculate the shortest combination of buttonEffects to get lights as an awnser
let findButtonPressesForLight machine =
    let rec findButtonPressesRec buttonIndex currentLights buttonPresses =
        match currentLights with 
        | _ when currentLights = machine.Lights -> [buttonPresses]
        | _ when buttonIndex >= machine.Buttons.Length -> []
        | _ ->
            let solutionsIfSkipped = findButtonPressesRec (buttonIndex+1) currentLights buttonPresses
            let solutionIfPressed = //Build the result add it to buttonPresses en call red fun again
                findButtonPressesRec 
                    (buttonIndex+1) 
                    (List.map2 (^^^) currentLights machine.Buttons.[buttonIndex]) 
                    (buttonPresses @ [machine.Buttons.[buttonIndex]])
        
            solutionIfPressed @ solutionsIfSkipped

    findButtonPressesRec 0 (List.replicate machine.Lights.Length 0) [] 
let partOne = 
    machines |>
    List.map findButtonPressesForLight |>
    List.map (fun solutions -> 
        solutions |> List.minBy List.length |> fun min -> min.Length
    ) |>
    List.sum

printf "Part one: %d\n" partOne
