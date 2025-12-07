#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let parsedInput = 
    input |> 
    List.map (fun line -> 
        line.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    )

let useOp op a b=
    match op with
    | "*" -> a * b
    | "+" -> a + b
    | _ -> failwith "Unknown operator"

let opsStack = 
    parsedInput |>
    List.filter (fun line -> 
        line |> List.forall (fun str ->
            match Int64.TryParse str with
            | (true, value) -> false
            | (false, _)            -> true
        )
    ) |>
    List.collect id

let numberLines = 
    parsedInput |>
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

let padAndTranspose (stackLines: string list) =
    let maxWidth = stackLines |> List.map String.length |> List.max
    stackLines |> 
    List.map (fun s -> s.PadLeft(maxWidth, ' ')) |> 
    List.map Seq.toList |> 
    transpose |> 
    List.map (fun chars -> 
        chars |> 
        List.filter (fun c -> c <> ' ') |> 
        String.Concat
    )

let numberStacksTwo (stackLines) =
    let maxLineLength = stackLines |> List.map String.length |> List.max

    stackLines |>
    List.map (fun s -> s.PadRight(maxLineLength, ' ')) |>
    List.map Seq.toList


    // parsedInput |>
    // List.map (fun stackLine ->
    //     padAndTranspose stackLine
    // ) |>
    // List.map (fun subList ->
    //     subList |> List.map System.Int64.Parse
    // )
    
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

// let partTwo =
//     numberStacksTwo |>
//     List.mapi (fun i numsLine ->
//         let op = opsStack.[i]
//         numsLine |> 
//         List.reduce (fun a b -> 
//             useOp op a b
//         )    
//     ) |>
//     List.sum

printf "%A\n" partOne
