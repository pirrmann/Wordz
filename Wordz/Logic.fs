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

let getBoundaries (forbiddenPixels:bool[,]) =
    let rec walk isInRange last start current = seq {
        if current > last then
            for x in start .. current - 1 do
                yield x, current
        elif isInRange current then
            // all previous that were not in range map to current
            for x in start .. current do
                yield x, current
            // then move to the next
            yield! walk isInRange last (current + 1) (current + 1)
        else // let's look further !
            yield! walk isInRange last start (current + 1)
    }

    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)

    let lastX = width - 1
    let lastY = height - 1

    let nextFreeRight = Array2D.zeroCreate width height
    let nextInvalidRight = Array2D.zeroCreate width height
    let nextFreeBottom = Array2D.zeroCreate width height
    let nextInvalidBottom = Array2D.zeroCreate width height

    for y in 0 .. lastY do
        let canUse current = not(forbiddenPixels.[current, y])

        for x, right in walk canUse lastX 0 0 do
            nextFreeRight.[x,y] <- right
        for x, right in walk (not << canUse) lastX 0 0 do
            nextInvalidRight.[x,y] <- right

    for x in 0 .. lastX do
        let canUse current = not(forbiddenPixels.[x, current])

        for y, bottom in walk canUse lastY 0 0 do
            nextFreeBottom.[x,y] <- bottom
        for y, bottom in walk (not << canUse) lastY 0 0 do
            nextInvalidBottom.[x,y] <- bottom

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
     ConsecutiveFailuresCount: int
}

let addWord (targetColors: Color[,]) (state:AddingState) (index, word, textCandidates: TextCandidate list) =
    printfn "Word %d: %s" index word

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

            {
                ForbiddenPixels = state.ForbiddenPixels
                WordsSpots = spot :: state.WordsSpots
                ConsecutiveFailuresCount = 0
            }
        | None ->
            printfn "No spot found :("
            { state with ConsecutiveFailuresCount = state.ConsecutiveFailuresCount + 1 }

    printfn "Elapsed time in seconds: %f" watch.Elapsed.TotalSeconds

    newState

let addWords targetColors state words =
    let stateWithWordsAdded =
        words
        |> Seq.scan (addWord targetColors) state
        |> Seq.takeWhile (fun s -> s.ConsecutiveFailuresCount < 10)
        |> Seq.last
    { stateWithWordsAdded with ConsecutiveFailuresCount = 0 }

let generate (inputFolder:string, outputFolder:string) (inputFile, wordSets) =

    let target = Bitmap.FromFile(inputFolder + inputFile) :?> Bitmap
    let targetColors = getColors target
    let empty = new Bitmap(target.Width, target.Height)

    let result =
        let startingState =
            {
                ForbiddenPixels = Array2D.init target.Width target.Height (fun x y -> targetColors.[x, y].A < 15uy)
                WordsSpots = []
                ConsecutiveFailuresCount = 0
            }

        let finalState = wordSets |> Seq.fold (addWords targetColors) startingState

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
