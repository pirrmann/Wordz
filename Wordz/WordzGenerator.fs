module WordzGenerator

open System.IO

type TaskToProcess = {
    Input : FileInfo
    Output : FileInfo
    FontSizes : int list
    Words : string list
}

let buildWordSet task =
    task.Words |> List.map (fun word -> word, TextCandidates.buildTestCandidates word task.FontSizes)

let generate spotsFinder colorPicker task =
        let inputFilePath = task.Input.FullName
        let outputFilePath = task.Output.FullName
        let wordset = task |> buildWordSet
        ImageGenerator.generate spotsFinder colorPicker (inputFilePath, outputFilePath) wordset

let readWordsFromFile filePath =
    System.IO.File.ReadAllLines(filePath) |> Seq.toList