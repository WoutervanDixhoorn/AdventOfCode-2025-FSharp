#load "../Common/utils.fsx"

open Utils

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInputAsOneLine()

let isInvalidIdOne (id: string): bool =
    if id.Length % 2 <> 0 then false
    else id.[0..(id.Length/2)-1] = id.[id.Length/2..]

//Go trough range and find invalidIds
let processRangeOne (range: int64 list) = range |> List.filter (fun id -> isInvalidIdOne (string id))

let isInvalidIdTwo (id: string): bool =
    if id.Length < 2 then false
    else seq { 1 .. id.Length / 2 } 
        |> Seq.exists ( fun patternLen -> 
            if id.Length % patternLen <> 0 then false
            else String.replicate (id.Length / patternLen) id.[0 .. patternLen - 1] = id
        )

//Go trough range and find invalidIds
let processRangeTwo (range: int64 list) = range |> List.filter (fun id -> isInvalidIdTwo (string id))

let parseRange (range: string): int64 list = 
    let parsed = range.Split('-')
    [int64 parsed.[0] .. int64 parsed.[1]]
    
let PartOne = 
    input.Split(',')
    |> Seq.map parseRange // [ [beg..end], [beg..end], ....]
    |> Seq.map processRangeOne // [[id, id, id], [id, id, ...], ...]
    |> Seq.concat // [invalid_id, invalid_id, invalid_id, ....]
    |> Seq.sum

let PartTwo = 
    input.Split(',')
    |> Seq.map parseRange // [ [beg..end], [beg..end], ....]
    |> Seq.map processRangeTwo // [[id, id, id], [id, id, ...], ...]
    |> Seq.concat // [invalid_id, invalid_id, invalid_id, ....]
    |> Seq.sum

printf "Part one: %d\n" PartOne
printf "Part two: %d\n" PartTwo