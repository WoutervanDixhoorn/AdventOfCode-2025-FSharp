#load "../Common/utils.fsx"
open Utils

#r "nuget: FParsec"
open FParsec

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

#r "nuget: Google.OrTools"
open Google.OrTools.LinearSolver

let calcMachineOrTools machine =
    let solver = Solver.CreateSolver("SCIP")
    let integerVars = 
        machine.Buttons |> List.mapi (fun i _ -> 
            solver.MakeIntVar(0.0, infinity, sprintf "Button_%d" i)
        )

    machine.JReq |> List.iteri (fun jIdx joltage -> 
        let target = float joltage
        let constraintObj = solver.MakeConstraint(target, target)
        machine.Buttons |> List.iteri (fun bIdx button ->
            let buttonEffect = button.[jIdx]
            constraintObj.SetCoefficient(integerVars.[bIdx], buttonEffect)
        )
    )

    let objective = solver.Objective()
    integerVars |> List.iter (fun var -> 
        objective.SetCoefficient(var, 1.0)
    )
    objective.SetMinimization()

    match solver.Solve() with
    | Solver.ResultStatus.OPTIMAL -> integerVars |> List.map (fun v -> int (v.SolutionValue())) |> List.sum
    | _ -> failwith "No solution"

let partTwo = machines |> List.map calcMachineOrTools |> List.sum

printfn "Part two: %A\n" partTwo
