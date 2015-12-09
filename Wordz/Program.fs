open System.IO
open WordzGenerator

let coolWords =
    [
        "FSharp"; "Paris"; "dojo"; "kata"; "Alice"; "Wonderland"; "code";
        "cards"; "doublets"; "alphabet"; "cypher"; "number"; "game"
        "@pirrmann"; "@brandewinder"; "@thinkb4coding"; "computation"; "type";
        "fun"; "async"; "compose"; ">>"; "|>"; "tuple"; "union"; "record"
        "magic"; "square"; "fox"; "goose"; "bag"; "corn"; "love"; "skills"
        "functions"; "map"; "filter"; "choose"; "let"; "rec"; "seq"
    ]

let fontSizes =
    [32 .. -4 .. 24] @
    [22 .. -2 .. 12] @
    [11 .. -1 .. 4]

let inputFolder = @"..\..\..\"
let outputFolder = @"..\..\..\"

let inputFile = "Sample.png"

let testWith spotsFinder =
    let sw = System.Diagnostics.Stopwatch.StartNew()

    System.IO.Directory.CreateDirectory(outputFolder) |> ignore
    let inputFilePath = System.IO.Path.Combine(inputFolder, inputFile)
    let outputFilePath =
        System.IO.Path.Combine(
            outputFolder,
            Path.GetFileNameWithoutExtension(inputFile) + "_wordz" + Path.GetExtension(inputFile))
    
    {
        Input  = FileInfo(inputFilePath)
        Output  = FileInfo(outputFilePath)
        FontSizes = fontSizes
        Words = coolWords
    } |> WordzGenerator.generate spotsFinder

    sw.Elapsed

[<EntryPoint>]
let main argv =
    let sw = System.Diagnostics.Stopwatch.StartNew()

    let def = testWith (new DefaultLogic.DefaultSpotFinder(true))
    let fox = testWith (new FoxLogic.FoxSpotFinder(true))

    printfn "Elapsed default : %O" def
    printfn "Elapsed fox : %O" fox
    
    System.Console.ReadLine() |> ignore

    0
