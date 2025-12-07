#load "../Common/utils.fsx"

open Utils
open System
open System.Collections.Generic

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let visited = new HashSet<int * int>()
    
let rec parseManuscriptPartOne (remainingLines: string list) lineIndex colIndex =
    match remainingLines with
    | [] -> 0   
    | currentLine :: rest ->  
        if colIndex < 0 || lineIndex >= currentLine.Length then 0

        else if visited.Contains(lineIndex, colIndex) then 0

        else
            visited.Add(lineIndex, colIndex) |> ignore

            match currentLine.[colIndex] with
            | 'S'
            | '.' ->
                parseManuscriptPartOne rest (lineIndex + 1) colIndex 
            | '^' ->
                1 + 
                parseManuscriptPartOne rest (lineIndex + 1) (colIndex - 1) + 
                parseManuscriptPartOne rest (lineIndex + 1) (colIndex + 1)
            | _ -> 0

let memo = Dictionary<(int *int), int64>()

let rec parseManuscriptPartTwo (remainingLines: string list) rowIndex colIndex =
    match remainingLines with
    | [] -> 1L
    | currentLine :: rest ->
        if memo.ContainsKey((rowIndex, colIndex)) then memo.[(rowIndex, colIndex)]
        else
            let result = 
                if colIndex < 0 || colIndex >= currentLine.Length then 0L
                else
                    match currentLine.[colIndex] with
                    | 'S' | '.' -> 
                        parseManuscriptPartTwo rest (rowIndex + 1) colIndex
                    | '^' -> 
                        parseManuscriptPartTwo rest (rowIndex + 1) (colIndex - 1) + 
                        parseManuscriptPartTwo rest (rowIndex + 1) (colIndex + 1)
                    | _ -> 0L
            memo.Add((rowIndex, colIndex), result)
            result             

let firstIndex = 
    input |>
    List.head |> Seq.toList |>
    Seq.tryFindIndex ((=) 'S') |> 
    Option.defaultValue 0

let splitCount = parseManuscriptPartOne (input |> List.tail) 0 firstIndex
let pathCount = parseManuscriptPartTwo (input |> List.tail) 0 firstIndex

printf "PartOne: %d\n" splitCount
printf "PartTwo: %d\n" pathCount