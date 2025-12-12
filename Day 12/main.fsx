#load "../Common/utils.fsx"
open Utils

#r "nuget: FParsec"
open FParsec

#r "nuget: Google.OrTools"
open Google.OrTools.Sat

let PARALLEL_BATCH_SIZE = 4 
let TIMEOUT_SECONDS = 30.0

//Run using: Get-Content input.txt | dotnet fsi main.fsx
let input = readInputAsOneLine()

let gridCell = (pchar '#' >>% 1) <|> (pchar '.' >>% 0)
let gridRow = many1 gridCell .>> newline
let parseShape = attempt (
    pipe2
        (pint32 .>> pstring ":" .>> newline)
        (many1 gridRow)
        (fun id grid -> (id, grid))
)
let parseTree =
    pipe3
        (pint32 .>> pchar 'x')
        (pint32 .>> pstring ":")
        (spaces >>. sepBy pint32 (pchar ' ') .>> newline) 
        (fun w h vals -> ((w, h), vals))

let parseFullFile =
    pipe2
        (many (parseShape .>> spaces))
        (many parseTree) // spaces is handled inside parseInstruction now
        (fun shapes instrs -> (shapes, instrs))

let parsedInput =
    match run parseFullFile (input + "\n") with
    | Success(result, _, _) -> result
    | Failure(err, _, _) -> failwith err

let (shapesList, treeList) = parsedInput

let shapes = shapesList |> List.map (fun (_, grid) -> grid)
let trees =  treeList

let shapeAreas = shapes |> List.map (fun s -> s |> List.sumBy List.sum)

let passesAreaCheck ((w, h), shapeAmounts) =
    shapeAmounts |> 
    List.mapi (fun idx amount -> amount * shapeAreas.[idx]) |> 
    List.sum |> 
    fun total -> total <= (w * h)

let filteredTrees = trees |> List.filter passesAreaCheck 

printf "Part One %A\n" filteredTrees.Length