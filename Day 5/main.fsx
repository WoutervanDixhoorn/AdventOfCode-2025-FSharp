#load "../Common/utils.fsx"

open Utils
open System

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInput()

let validIngredients =
    input |> List.filter (fun elem ->
        match Int64.TryParse elem with
        | (true, _) -> false
        | (_, _)    -> true
    ) |>
    List.choose (fun elem -> 
        match elem.Split('-') with
        | [|s; f|] -> Some(int64 s, int64 f)
        | _ -> None
    )

let validIngredientsCount =
    let sortedRanges = validIngredients |> List.sortBy fst

    let rec merge currentRange remainingRanges acc =
        match remainingRanges with
        | [] -> currentRange :: acc
        | (nextStart, nextEnd) :: tail ->
            let (currStart, currEnd) = currentRange
            if nextStart <= currEnd + 1L then
                let newEnd = max currEnd nextEnd
                merge (currStart, newEnd) tail acc
            else
                merge (nextStart, nextEnd) tail (currentRange :: acc)

    match sortedRanges with
    | head :: tail ->
        let mergedRanges = merge head tail []
        mergedRanges |> 
        List.sumBy (fun (s, f) -> f - s + 1L)
    | _ -> 0L


let isInRange elem start finish =
    elem >= start && elem <= finish

let ingredients = 
    input |> List.filter (fun elem ->
        match Int64.TryParse elem with
        | (true, _) -> true
        | (_, _)    -> false
    ) |> 
    List.map (fun elem -> int64 elem) |>
    List.filter (fun elem -> 
        validIngredients |> List.exists (fun (start, finish) -> isInRange elem start finish)
    ) |>
    List.length


printf "%A" validIngredientsCount