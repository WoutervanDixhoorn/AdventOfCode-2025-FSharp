#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let parsedInputOne = 
    input |> 
    List.map (fun line -> 
        line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |>
        Array.toList
    )

let useOp op a b=
    match op with
    | "*" -> a * b
    | "+" -> a + b
    | _ -> failwith "Unknown operator"

let opsStack = 
    parsedInputOne |>
    List.filter (fun line -> 
        line |> List.forall (fun str ->
            match Int64.TryParse str with
            | (true, value) -> false
            | (false, _)    -> true
        )
    ) |>
    List.collect id

let numberLines = 
    parsedInputOne |>
    List.filter (fun line -> 
        line |> List.forall (fun str ->
            match Int64.TryParse str with
            | (true, value) -> true
            | (false, _)            -> false
        )
    ) |>
    List.transpose   

let numberStack = 
    numberLines |>
    List.map (fun subList ->
        subList |> List.map System.Int64.Parse
    )

let rec transpose charList =
        match charList |> List.filter (fun row -> not (List.isEmpty row)) with
        | [] -> []
        | rows ->
            let heads    = List.map List.head rows 
            let tails = List.map List.tail rows
            heads :: transpose tails
    
let partOne = 
    numberStack |>
    List.mapi (fun i numsLine ->
        let op = opsStack.[i]
        numsLine |> 
        List.reduce (fun a b -> 
            useOp op a b
        )    
    ) |>
    List.sum

printf "PartOne: %A\n" partOne

//Part two 
//1. Read input
//2. Chunk input into problem blocks
//3. transpose
//4. reduce

let normalizeGrid (lines: string list) =
    let maxLen = lines |> List.map (fun s -> s.Length) |> List.max
    lines |> List.map (fun s -> s.PadRight(maxLen, ' '))

let paddedInput = normalizeGrid input
let columns = 
    paddedInput
    |> List.take (paddedInput.Length - 1) 
    |> List.map Seq.toList 
    |> transpose

let isSpace col = 
    col |> List.forall (fun c -> c = ' ')

let problemBlocksChars =
    let (lastBlock, collectedBlocks) = 
        columns |>
        List.fold (fun (currentBlock, allBlocks) col ->
            if isSpace col then 
                ([], currentBlock :: allBlocks)
            else
                (col :: currentBlock, allBlocks)
        ) ([],[]) 
    (lastBlock :: collectedBlocks)

let problemBlocksNums = 
    problemBlocksChars |>
    List.map (fun blocks ->
        blocks |>
        List.map Seq.toList |>
        List.map String.Concat |>
        List.map int64  
    )

let partTwo =
    problemBlocksNums |>
    List.mapi (fun i block ->
        let opReversed = opsStack |> List.rev
        block |>
        List.reduce (fun a b -> 
            useOp opReversed.[i] a b
        ) 
    ) |> 
    List.sum

printf "PartTwo: %A\n" partTwo