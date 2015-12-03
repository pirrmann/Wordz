module Logic

open System.Drawing
open System.Drawing.Imaging
    
let getColors (bitmap:Bitmap) =
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

type TextCandidate = {
    Text: string
    FontSize: int
    Width: int
    Height: int
    Pixels: bool[,]
}

let getFont size = new Font("Arial", float32 size)

let buildTestCandidates word fontSizes =
    use bit = new Bitmap(1, 1)
    use measureGraphics = bit |> Graphics.FromImage
    measureGraphics.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    let buildTestCandidate fontSize =
        use font = getFont fontSize
        let size = measureGraphics.MeasureString(word, font)
        let width, height = int size.Width, int size.Height
        use bitmap = new Bitmap(width, height)
        use g = bitmap |> Graphics.FromImage
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        g.DrawString(word, font, Brushes.Black, 0.f, 0.f)
        {
            Text = word
            FontSize = fontSize
            Width = width
            Height = height
            Pixels = Array2D.init width height (fun x y -> bitmap.GetPixel(x, y).A > 15uy)
        }

    fontSizes |> List.map buildTestCandidate
    
type Spot = {
    X:int
    Y:int
    TextCandidate: TextCandidate
}

type Boundaries = {
    Width: int
    Height: int
    NextInvalidRight: int[,]
    NextFreeRight: int[,]
    NextInvalidBottom: int[,]
    NextFreeBottom: int[,] }

let groupConsecutive input = seq {
        let mutable currentGroup : option<int * int * _>  = None
        for elem in input do
            match currentGroup with
            | None ->
                currentGroup <- Some (0, 1, elem)
            | Some (start, n, value) ->
                if value = elem then
                    currentGroup <- Some (start, n+1, value)
                else
                    yield start, n, value
                    currentGroup <- Some (start+n, 1, elem)

        match currentGroup with
        | None -> ()
        | Some g -> yield g
    }

let indexesOfNextTrueAndFalse length repetitions = seq {
    for (start, count, value) in repetitions do
        for i in start..start+count-1 do
            if value then
                yield (i, start+count)
            else
                yield (start+count, i)
    }

let getBoundaries (forbiddenPixels:bool[,]) =
    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)

    let lastX = width - 1
    let lastY = height - 1

    let nextFreeRight = Array2D.zeroCreate width height
    let nextInvalidRight = Array2D.zeroCreate width height
    let nextFreeBottom = Array2D.zeroCreate width height
    let nextInvalidBottom = Array2D.zeroCreate width height

    for y in 0 .. lastY do
        let nextTrueAndFalseOnLine =
            seq { for x in 0..lastX do yield forbiddenPixels.[x, y] }
            |> groupConsecutive
            |> indexesOfNextTrueAndFalse width
            |> Seq.mapi (fun i value -> i, value)

        for x, (nextTrue, nextFalse) in nextTrueAndFalseOnLine do
            nextFreeRight.[x,y] <- nextFalse
            nextInvalidRight.[x,y] <- nextTrue

    for x in 0 .. lastX do
        let nextTrueAndFalseOnColumn =
            seq { for y in 0..lastY do yield forbiddenPixels.[x, y] }
            |> groupConsecutive
            |> indexesOfNextTrueAndFalse width
            |> Seq.mapi (fun i value -> i, value)

        for y, (nextTrue, nextFalse) in nextTrueAndFalseOnColumn do
            nextFreeBottom.[x,y] <- nextFalse
            nextInvalidBottom.[x,y] <- nextTrue

    {
        Width = width
        Height = height
        NextFreeRight = nextFreeRight
        NextInvalidRight = nextInvalidRight
        NextFreeBottom = nextFreeBottom
        NextInvalidBottom = nextInvalidBottom
    }

type FitResult =
    | Fits
    | CannotFitUntilX of int

let canFit boundaries (width, height) (x, y)  =
    let blockingPixelsRight =
        lazy ([| y .. min (y + height) (boundaries.Height - 1) |]
              |> Array.map (fun y -> boundaries.NextInvalidRight.[x, y], y)
              |> Array.filter (fun (right, _) -> right < x + width))
    let blockingPixelsBottom =
        lazy ([| x .. min (x + width) (boundaries.Width - 1) |]
              |> Array.map (fun x -> x, boundaries.NextInvalidBottom.[x, y])
              |> Array.filter (fun (_, bottom) -> bottom < y + height))
    
    if blockingPixelsRight.Value.Length > 0 then
        let blockingX, blockingY = Array.maxBy fst blockingPixelsRight.Value
        let nextFreeX = if blockingX < boundaries.NextFreeRight.GetLength(0) && blockingY < boundaries.NextFreeRight.GetLength(1)
                        then boundaries.NextFreeRight.[blockingX, blockingY]
                        else blockingX
        CannotFitUntilX nextFreeX
    elif blockingPixelsBottom.Value.Length > 0 then
        let blockingX, blockingY = Array.maxBy fst blockingPixelsBottom.Value
        let nextFreeX = if blockingX < boundaries.NextFreeRight.GetLength(0) && blockingY < boundaries.NextFreeRight.GetLength(1)
                        then boundaries.NextFreeRight.[blockingX, blockingY]
                        else blockingX
        CannotFitUntilX nextFreeX
    else
        Fits

type AddingState = {
     ForbiddenPixels: bool[,]
     WordsSpots: Spot list
     RemainingWords: (string * TextCandidate list) list
     NextIterationWords: (string * TextCandidate list) list
}

let addWord (targetColors: Color[,]) (state:AddingState) =
    let word, textCandidates = state.RemainingWords.Head

    printfn "Word %s" word

    let watch = System.Diagnostics.Stopwatch.StartNew()

    let boundaries = getBoundaries state.ForbiddenPixels

    printfn "Elapsed time in seconds: %f" watch.Elapsed.TotalSeconds

    let rec findSpot (width, height) (x, y) =
        if y >= boundaries.Height then None
        elif x >= boundaries.Width then findSpot (width, height) (0, y + 1)
        else
            match canFit boundaries (width, height) (x, y) with
            | Fits ->
                Some (x, y)
            | CannotFitUntilX nextX ->
                findSpot (width, height) (nextX, y)

    let spots = seq {
        for textCandidate in textCandidates do

            match findSpot (textCandidate.Width, textCandidate.Height) (0, 0) with
            | Some (x, y) ->
                yield {
                    X = x
                    Y = y
                    TextCandidate = textCandidate
                }
            | None -> ()
        }

    let bestSpot = spots |> Seq.tryHead
    
    let newState =
        match bestSpot with
        | Some spot ->
            printfn "Found a spot! %A, size %d" (spot.X, spot.Y) spot.TextCandidate.FontSize

            for x in 0 .. spot.TextCandidate.Width - 1 do
                for y in 0 .. spot.TextCandidate.Height - 1 do
                    state.ForbiddenPixels.[spot.X + x, spot.Y + y] <- state.ForbiddenPixels.[spot.X + x, spot.Y + y] || spot.TextCandidate.Pixels.[x, y]

            let remainingCandidates = textCandidates |> List.skipWhile (fun c -> c <> spot.TextCandidate)

            {
                ForbiddenPixels = state.ForbiddenPixels
                WordsSpots = spot :: state.WordsSpots
                RemainingWords = state.RemainingWords.Tail
                NextIterationWords = (word, remainingCandidates) :: state.NextIterationWords
            }

        | None ->
            printfn "No spot found :("
            { state with RemainingWords = state.RemainingWords.Tail }

    printfn "Elapsed time in seconds: %f" watch.Elapsed.TotalSeconds

    newState

let rand =
    let r = new System.Random(42)
    fun () -> r.NextDouble()

let rec addWords targetColors (state:AddingState) =
    match state.RemainingWords, state.NextIterationWords with
    | [], [] -> state
    | [], nextIterationWords ->
        let remainingWords = nextIterationWords |> List.sortBy (fun _ -> rand())
        let state' = { state with RemainingWords = remainingWords
                                  NextIterationWords = [] }
        addWords targetColors state'
    | _ ->
        let state' = addWord targetColors state
        addWords targetColors state'

let generate (inputFolder:string, outputFolder:string) (inputFile, words) =

    let target = Bitmap.FromFile(inputFolder + inputFile) :?> Bitmap
    let targetColors = getColors target
    let empty = new Bitmap(target.Width, target.Height)

    let result =
        let startingState =
            {
                ForbiddenPixels = Array2D.init target.Width target.Height (fun x y -> targetColors.[x, y].A < 15uy)
                WordsSpots = []
                RemainingWords = []
                NextIterationWords = words
            }

        let finalState = addWords targetColors startingState

        let result = new Bitmap(target.Width, target.Height)
        use g = Graphics.FromImage result
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        for spot in finalState.WordsSpots do
            let color = targetColors.[spot.X + spot.TextCandidate.Width / 2, spot.Y + spot.TextCandidate.Height / 2]
            use font = getFont spot.TextCandidate.FontSize
            use brush = new SolidBrush(color)
            printfn "drawing text %s" spot.TextCandidate.Text
            g.DrawString(spot.TextCandidate.Text, font, brush, single spot.X, single spot.Y)

        result

    result.Save(outputFolder + inputFile, ImageFormat.Png)
