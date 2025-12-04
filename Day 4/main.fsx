#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let binaryInput = 
    input |>
    Seq.map (fun line ->
        line |> Seq.map (fun  ch -> 
            match ch with
            | '.' -> 0
            | '@' -> 1
            | _   -> 0
        ) |> Seq.toList
    ) |> Seq.toList

let padInput (grid: int list list) =
    let rows = grid.Length
    let cols = grid.[0].Length
    
    let zeroRow = List.replicate (cols+2) 0
    let paddedRows = 
        grid |>
        List.map (fun row -> [0] @ row @ [0])

    [zeroRow] @ paddedRows @ [zeroRow]

let applyKernel window kernel =
    List.map2 (fun k pixel -> k * pixel) kernel window 
    |> List.sum

let kernelOne = [
    1;1;1;
    1;0;1;
    1;1;1
]

let convolve kernel input =
    input |>
    List.windowed 3 |>
    List.map (fun threeRows ->
        threeRows |>
        List.transpose |>
        List.windowed 3 |>
        List.map (fun block -> 
            let flatBlock = List.concat block
            applyKernel flatBlock kernel
        )
    )

let convolveOne = convolve kernelOne

let countRemovals binaryInput convoled = 
    let flattenInput = List.concat binaryInput
    let flattenConvolved = List.concat convoled
    List.zip flattenInput flattenConvolved
    |> List.filter (fun (isAlive, neighbours) -> isAlive = 1 && neighbours < 4)
    |> List.length

let removeReachable (unpaddedInput) =
    let padded = padInput unpaddedInput
    let neighborCounts = convolveOne padded

    let nextGen =
        List.map2 (fun inputRow convolvedRow ->
            List.map2 (fun inputCell convolvedCell -> 
                if inputCell = 1 && convolvedCell < 4 then 
                    (0, 1)
                else (inputCell, 0)
            ) inputRow convolvedRow
        ) unpaddedInput neighborCounts

    let nextGrid = 
        nextGen |> List.map (List.map fst)
    let removedCount = 
        nextGen |> List.sumBy (List.sumBy snd)

    (nextGrid, removedCount)


let partTwo = 
    let rec countRemoves totalRemoved currentGrid =
        let (nextGrid, removalsInThisStep) = removeReachable currentGrid
        
        match removalsInThisStep with 
        | 0 -> totalRemoved
        | _ -> countRemoves (totalRemoved + removalsInThisStep) nextGrid
    
    countRemoves 0 binaryInput

let paddedInput = padInput binaryInput
let convolved = convolveOne paddedInput
let partOne = countRemovals binaryInput convolved

printf "Part One: %d\n" partOne
printf "Part Two: %d\n" partTwo