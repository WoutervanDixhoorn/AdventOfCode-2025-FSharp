module Utils

open System

let readInput () =
    Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun line -> line <> null)
        |> Seq.map (fun line -> line.Replace("\uFEFF", "").Trim())
        |> Seq.filter(fun line -> not (String.IsNullOrEmpty(line)))
        |> Seq.toList

let readInputAsOneLine () =
    Seq.initInfinite (fun _ -> Console.ReadLine())
        |> Seq.takeWhile (fun line -> line <> null)
        |> Seq.map (fun line -> line.Replace("\uFEFF", "").Trim())
        |> Seq.filter (fun line -> not (String.IsNullOrEmpty(line)))
        |> String.concat ""


let wrap n = ((n % 100) + 100) % 100