#load "../Common/utils.fsx"

open Utils
open System

let processLinePartOne (currentDial: int, currentCount: int) (line: string): int * int =
    let direction = line.[0..0]
    let amount = Int32.Parse(line.[1..])

    let newDial = 
        match direction with
        | "L" -> wrap (currentDial - amount)
        | "R" -> wrap (currentDial + amount)
        | _   -> currentDial

    let newCount = 
        if newDial = 0 then currentCount + 1
        else currentCount

    (newDial, newCount)

let getSector (position: int) =
    int (Math.Floor(float position / 100.0))    

let processLinePartTwo (currentDial: int, currentCount: int) (line: string): int * int =
    let direction = line.[0..0]
    let amount = Int32.Parse(line.[1..])

    match direction with
    | "R" -> 
        let nextDial = currentDial + amount
        let hits = getSector(nextDial) - getSector(currentDial)
        (nextDial, currentCount + hits)
    | "L" -> 
        let nextDial = currentDial - amount
        let hits = getSector(currentDial - 1) - getSector(nextDial - 1)
        (nextDial, currentCount + hits)
    | _ -> (currentDial, currentCount)


//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let (finalDial1, finalCount1 )= 
    input
    |> Seq.fold processLinePartOne (50, 0)
    
let (finalDial2, finalCount2 )= 
    input
    |> Seq.fold processLinePartTwo (50, 0)

printfn "---"
printfn "Final Result Part One: %d" finalCount1
printfn "---"
printfn "Final Result Part Two: %d" finalCount2

    