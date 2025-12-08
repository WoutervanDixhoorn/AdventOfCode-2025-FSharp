#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

type Position = {
    X: int64;
    Y: int64;
    Z: int64;
}

let juntionPositions =
    input |>
    List.map (fun line ->
        match line.Split(',') with
        | [|xpos; ypos; zpos|] -> {X=int64 xpos; Y= int64 ypos; Z=int64 zpos}
        | _ -> {X=0L; Y=0L; Z=0L}
    )

let calcDistance (a: Position, b: Position) =
    let dx = a.X - b.X
    let dy = a.Y - b.Y
    let dz  = a.Z - b.Z
    (dx*dx) + (dy*dy) + (dz*dz)
    
//DSU - First time learning about these, some help from my friend gemini
type DSU(count: int) =
    let parent = Array.init count id
    let size = Array.create count 1
    let mutable groups = count

    member this.FindBoss(i: int) = 
        if parent.[i] = i then
            i
        else 
            let boss = this.FindBoss(parent.[i])
            parent.[i] <- boss
            boss

    member this.Union(i: int, j:int) =
        let bossTeamA = this.FindBoss(i)
        let bossTeamB = this.FindBoss(j)

        if bossTeamA <> bossTeamB then
            // printfn "Connecting Team %d (Size %d) and Team %d (Size %d)"
            //     bossTeamA size.[bossTeamA] bossTeamB size.[bossTeamB]

            parent.[bossTeamB] <- bossTeamA
            size.[bossTeamA] <- size.[bossTeamA] + size.[bossTeamB]
            groups <- groups - 1
            true
        else
            //printfn "Boss %d and %d are already in the same team!" i j
            false
            
    member this.GetClusterSizes() =
        parent |>
        Array.mapi (fun i p ->
            if p = i  then size.[i] else 0
        ) |>
        Array.filter (fun s -> s > 0)

    member this.GetGroupCount() = 
        groups

let nodes = juntionPositions |> List.toArray
let nodesCount = nodes.Length

let allEdges =
    [|
        for i in 0 .. nodesCount - 1 do
            for j in i + 1 .. nodesCount - 1 do
                let distance = calcDistance(nodes.[i], nodes.[j])
                yield (distance, i, j)
    |]

let sortedEdges =
    allEdges |>
    Array.sortBy (fun dist -> dist)

let partOne = 
    let jBoxDsu = DSU(nodesCount)

    sortedEdges |>
    Array.truncate 1000 |>
    Array.iter (fun (_, idxA, idxB) -> 
        jBoxDsu.Union(idxA, idxB) |> ignore
    )
    
    let result =
        jBoxDsu.GetClusterSizes() |>
        Array.sortDescending |>
        Array.take 3 |>
        Array.map int64 |>
        Array.reduce (fun a b -> a * b)
    result
    
printf "PartOne: %A\n" partOne

//Kruskal's Algo
//1. Sorteer op afstand descending (laag -> hoog)
//2. Voeg kleinste toe mits deze een cyclus vormt (Mag dus niet aan edge die al in set zit)

let partTwo =
    let jBoxDsu = DSU(nodesCount)

    sortedEdges |>
    Array.pick (fun (_, idxA, idxB) ->
        if jBoxDsu.Union(idxA, idxB) && jBoxDsu.GetGroupCount() = 1 then
            Some(nodes.[idxA].X * nodes.[idxB].X)
        else
            None
    )
    
printf "PartTwo: %A\n" partTwo