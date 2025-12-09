#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

type FloorTile = {
    X: int;
    Y: int;
}

let floorTiles =
    input |>
    List.map (fun line ->
        match line.Split(',') with 
        | [|xpos; ypos|] -> {X=int xpos; Y=int ypos}
        | _ -> {X=0; Y=0}
    )

let partOne =
    floorTiles |>
    List.mapi (fun idx tileA ->
        floorTiles.[idx+1..] |>
        List.map (fun tileB-> 
            int64 (abs(tileA.X - tileB.X) + 1) * int64 (abs(tileA.Y - tileB.Y) + 1)
        )
    ) |>
    List.concat |>
    List.max

printf "PartOne: %A\n" partOne

type floorRect = {
    t1: FloorTile;
    t2: FloorTile;
    t3: FloorTile;
    t4: FloorTile;
    Area: int64;
}

let tilePairs = floorTiles |> List.pairwise //Pre calculate pairs

let isPointInside x y (redTiles: FloorTile list) =
    tilePairs |>
    List.fold (fun acc (tileA, tileB)  ->
        not ((tileA.Y > y) <> (tileB.Y > y) && tileA.X < x)
    ) false

let wallOverlapsRect rectX1 rectX2 rectY1 rectY2 (tileA: FloorTile, tileB: FloorTile) =
    if tileA.X = tileB.X then 
        let wallX  = tileA.X
        let wallY1 = min tileA.Y tileB.Y
        let wally2 =  max tileA.Y tileB.Y
        
        wallX > rectX1 && wallX < rectX2 && 
        max rectY1 wallY1 < min rectY2 wally2
    else 
        let wallY  = tileA.Y
        let wallX1 = min tileA.X tileB.X
        let wallX2 = max tileA.X tileB.X

        wallY > rectY1 && wallY < rectY2 && 
        max rectX1 wallX1 < min rectX2 wallX2

let partTwo =
    floorTiles |>
    List.mapi (fun idx tileA ->
        floorTiles.[idx+1..] |>
        List.choose (fun tileB ->
            let tileC = { X = tileA.X; Y = tileB.Y }
            let tileD = { X = tileB.X; Y = tileA.Y }

            let minX, maxX = min tileA.X tileB.X, max tileA.X tileB.X
            let minY, maxY = min tileA.Y tileB.Y, max tileA.Y tileB.Y
            let width = int64 (maxX - minX + 1)
            let height = int64 (maxY - minY + 1)

            let centerX = (minX + maxX) / 2
            let centerY = (minY + maxY) / 2
            
            let isCenterSafe = isPointInside centerX centerY floorTiles
            
            if not isCenterSafe then
                None 
            else
                let overlaps = 
                    tilePairs |> 
                    List.exists (fun (wallStart, wallEnd) -> 
                        wallOverlapsRect minX maxX minY maxY (wallStart, wallEnd)
                    )
                
                if overlaps then 
                    None
                else
                    Some { 
                        t1 = tileA; t2 = tileB; t3 = tileC; t4 = tileD; 
                        Area = width * height 
                    }
        ) 
    ) |> 
    List.concat |>
    List.maxBy (fun rect -> rect.Area) |>
    fun rect -> rect.Area


printf "PartTwo %A\n" partTwo