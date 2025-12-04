#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let maxJoltageBankOne (bank: string): int =
    let nums = 
        bank |> 
        Seq.map (fun c -> int(Char.GetNumericValue(c)) ) |>
        Seq.toList
    
    let rec findMaxPossible currentBest remainingList = 
        match remainingList with
            | [] | [_] -> currentBest

            | head :: tail ->
                let maxTail = List.max tail
                let pairValue = (head * 10) + maxTail

                let newBest = max currentBest pairValue

                findMaxPossible newBest tail 
    
    let maxJoltage = findMaxPossible 0 nums

    printf "%d\n" maxJoltage

    maxJoltage

let maxJoltageBankTwo (bank: string): int64 =
    let nums = 
        bank |> 
        Seq.mapi (fun i c -> int(Char.GetNumericValue(c)) ) |>
        Seq.toList

    let removals = bank.Length - 12
    let (stack, _) =
        (([], removals), nums) ||>
        List.fold (fun (stack, removals) jolt->

            let rec maxJolt stack removals = 
                match stack with
                    | top :: rest when removals > 0 && top < jolt ->
                        maxJolt rest (removals - 1)
                    | _ -> (stack, removals)
            
            let (maxJoltStack, newRemovals) = maxJolt stack removals
            
            (jolt :: maxJoltStack, newRemovals)
        )
    
    let resultString = 
        stack
        |> List.rev
        |> List.take 12
        |> List.map string
        |> String.Concat

    printf "%s\n" resultString
    int64 resultString
        
let partOne =
    input |>
    Seq.map maxJoltageBankOne |>
    Seq.sum

let partTwo =
    input |>
    Seq.map maxJoltageBankTwo |>
    Seq.sum

printf "%d" partTwo