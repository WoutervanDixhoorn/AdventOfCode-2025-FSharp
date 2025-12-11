#load "../Common/utils.fsx"

open Utils
open System
open System.Collections.Generic

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let devices =
    let devicesDict = Dictionary()

    input |>
    List.map (fun line ->
        match line.Split(":") with
        | [|name; outputs|] -> (name, outputs.Split(' ', StringSplitOptions.RemoveEmptyEntries) |> Seq.toList)
        | _ -> failwith "unreachable"
    ) |>
    List.iter devicesDict.Add

    devicesDict

//Part One only - I like this clean rec block ;P
let getAllPathsToFromOne endDevice beginDevice =
    let rec getAllPathsToFromRec currentDevice=
        match currentDevice with 
        | _ when devices.[currentDevice] |> List.contains endDevice -> 1
        | _   -> devices.[currentDevice] |> List.sumBy getAllPathsToFromRec
            
    getAllPathsToFromRec beginDevice
//printf "Part One %d\n" (getAllPathsToFromOne "out" "you")

let getAllPathsToFromPassing endDevice beginDevice mustPass =
    let memo = Dictionary<string * Set<string>, int64>()

    let rec getAllPathsToFromPassingRec currentDevice  remaingToPass=
        match memo.TryGetValue((currentDevice, remaingToPass)) with 
        | true, count -> count
        | false, _ ->
            let newRemainingToPass = Set.remove currentDevice remaingToPass

            let result =
                devices.[currentDevice] 
                |> List.sumBy (fun output -> 
                    match output with
                    | _ when output = endDevice -> if Set.isEmpty newRemainingToPass then 1L else 0L
                    | _ -> getAllPathsToFromPassingRec output newRemainingToPass                        
                )
            
            memo.Add((currentDevice, remaingToPass), result)
            result

    getAllPathsToFromPassingRec beginDevice (Set.ofList mustPass)

let getAllPathsToFrom endDevice beginDevice =
    getAllPathsToFromPassing endDevice beginDevice List.Empty

printf "Part One %d\n" (getAllPathsToFrom "out" "you")
printf "Part Two %d\n" (getAllPathsToFromPassing "out" "svr" ["dac"; "fft"])