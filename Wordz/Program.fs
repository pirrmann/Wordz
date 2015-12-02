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

let rand =
    let r = new System.Random()
    fun () -> r.NextDouble()

let makeInfiniteWordSeq wordsToUse =
    let rec repeatShuffled words = seq {
        yield! words |> Seq.sortBy (fun _ -> rand())
        yield! repeatShuffled words
    }
    repeatShuffled wordsToUse
    |> Seq.mapi (fun id word -> id, word)

//let words = coolWords |> List.mapi (fun id word -> id, word)


let inputFolder = @"C:\Users\Pierre\Pictures\LevisIdeas\"
let outputFolder = @"C:\Code\"

let inputFiles =
    [
        "bobby-layer-1.png"
        //"bobby-layer-2.png"
        //"bobby-layer-3.png"
        //"bobby-layer-4.png"
        //"bobby-layer-5.png"
        //"bobby-layer-6.png"
    ]

let wordSets =
    [
        //readWordsFromFile @"C:\Users\Pierre\Pictures\LevisIdeas\words.txt" |> makeInfiniteWordSeq
        //[ "ITG"; "bob" ] |> makeInfiniteWordSeq
        //[ "€"; "$"; "£" ] |> makeInfiniteWordSeq
        [ 1, "a" ]
    ]

[<EntryPoint>]
let main argv =
    for inputFile in inputFiles do
        Logic.generate (inputFolder, outputFolder) (inputFile, wordSets)
    0
