let coolWords =
    [
        "FSharp"; "Paris"; "dojo"; "kata"; "Alice"; "Wonderland"; "code";
        "cards"; "doublets"; "alphabet"; "cypher"; "number"; "game"
        "@pirrmann"; "@brandewinder"; "@thinkb4coding"; "computation"; "type";
    
        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"

        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"
        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"
    ]

let readWordsFromFile filePath =
    System.IO.File.ReadAllLines(filePath) |> Seq.toList

let fontSizes = [32 .. -4 .. 24] @ [22 .. -2 .. 12] @ [11 .. -1 .. 4]

let buildWordSet wordsToUse =
    wordsToUse |> List.map (fun word -> word, Logic.buildTestCandidates word fontSizes)

let inputFolder = @"C:\Users\Pierre\Pictures\LevisIdeas\"
let outputFolder = @"C:\Code\"

let inputFiles =
    [
        "bobby-layer-1.png"
        "bobby-layer-2.png"
        "bobby-layer-3.png"
        "bobby-layer-4.png"
        "bobby-layer-5.png"
        "bobby-layer-6.png"
        "bobby-layer-7.png"
        "bobby-layer-8.png"
        "bobby-layer-9.png"
        "bobby-layer-10.png"
    ]

let words =
    [
        readWordsFromFile @"C:\Users\Pierre\Pictures\LevisIdeas\words.txt" 
        [ "ITG"; "bob"; "€"; "$"; "£" ]
    ] |> List.collect buildWordSet

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let tasks =
        [|
            for inputFile in inputFiles do
                yield async { Logic.generate (inputFolder, outputFolder) (inputFile, words) }
        |]
        |> Async.Parallel
        |> Async.RunSynchronously
    printf "Elapsed : %O" sw.Elapsed
    0
