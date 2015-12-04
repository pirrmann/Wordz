let coolWords =
    [
        "FSharp"; "Paris"; "dojo"; "kata"; "Alice"; "Wonderland"; "code";
        "cards"; "doublets"; "alphabet"; "cypher"; "number"; "game"
        "@pirrmann"; "@brandewinder"; "@thinkb4coding"; "computation"; "type";
        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"
    ]

let readWordsFromFile filePath =
    System.IO.File.ReadAllLines(filePath) |> Seq.toList

let fontSizes =
    [32 .. -4 .. 24] @
    [22 .. -2 .. 12] @
    [11 .. -1 .. 4]

let buildWordSet fontSizes wordsToUse =
    wordsToUse |> List.map (fun word -> word, Logic.buildTestCandidates word fontSizes)

let inputFolder = @"C:\Users\Pierre\Pictures\LevisIdeas"
let outputFolder = @"C:\Code"

let inputFiles =
    [
        "bobby-layer-1.png", true
        "bobby-layer-2.png", true
        "bobby-layer-3.png", true
        "bobby-layer-4.png", true
        "bobby-layer-5.png", true
        "bobby-layer-6.png", true
        "bobby-layer-7.png", true
        "bobby-layer-8.png", true
        "bobby-layer-9.png", true
        "bobby-layer-10.png", true
        "bobby-layer-11.png", true
        "bobby-layer-12.png", true
        "bobby-layer-13.png", true
        "bobby-layer-14.png", false
    ]

let bigWordsFromFile = readWordsFromFile @"C:\Users\Pierre\Pictures\LevisIdeas\words.txt" |> buildWordSet fontSizes

let wordsWithBigCurrencies =
    [
        bigWordsFromFile
        [ "€"; "$"; "£" ] |> buildWordSet fontSizes
    ] |> List.collect id

let wordsWithOnlySmallCurrencies =
    [
        bigWordsFromFile
        [ "€"; "$"; "£" ] |> buildWordSet (List.filter (fun size -> size <= 14) fontSizes)
    ] |> List.collect id

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let tasks =
        [|
            for inputFile, useBigCurrencies in inputFiles do
                let words = if useBigCurrencies then wordsWithBigCurrencies else wordsWithOnlySmallCurrencies
                yield async { Logic.generate (inputFolder, outputFolder) true (inputFile, words) }
        |]
        |> Async.Parallel
        |> Async.RunSynchronously
    printfn "Elapsed : %O" sw.Elapsed
    0
