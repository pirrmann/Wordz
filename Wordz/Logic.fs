module Logic

open System.Drawing
open System.Drawing.Imaging
    
let fontSizes = [32 .. -4 .. 24] @ [22 .. -2 .. 10] @ [11 .. -1 .. 6]

let getColors (bitmap:Bitmap) =
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

type Spot = {
    X:int
    Y:int
    FontSize:int
    Width:int
    Height:int
}

type Boundaries = {
    Width: int
    Height: int
    NextInvalidRight: int[,]
    NextFreeRight: int[,]
    NextInvalidBottom: int[,]
    NextFreeBottom: int[,] }

let getBoundaries (targetColors:Color[,]) (drawnPixels:Color[,]) =
    let rec walk isInRange last start current = seq {
        if current > last then
            for x in start .. current - 1 do
                yield x, current
        elif isInRange current then
            // all previous that were not in range map to current
            for x in start .. current do
                yield x, current
            // then move to the next
            yield! walk isInRange last (current+1) (current + 1)
        else // let's look further !
            yield! walk isInRange last start (current + 1)
    }

    let width = targetColors.GetLength(0)
    let height = targetColors.GetLength(1)

    let lastX = width - 1
    let lastY = height - 1

    let nextFreeRight = Array2D.zeroCreate width height
    let nextInvalidRight = Array2D.zeroCreate width height
    let nextFreeBottom = Array2D.zeroCreate width height
    let nextInvalidBottom = Array2D.zeroCreate width height

    for y in 0 .. lastY do
        let canUse current =
            targetColors.[current, y].A > 15uy
            && drawnPixels.[current, y].A < 15uy

        for x, right in walk canUse lastX 0 0 do
            nextFreeRight.[x,y] <- right
        for x, right in walk (not << canUse) lastX 0 0 do
            nextInvalidRight.[x,y] <- right

    for x in 0 .. lastX do
        let canUse current =
            targetColors.[x, current].A > 15uy
            && drawnPixels.[x, current].A < 15uy

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

let getFont size = new Font("Arial", float32 size)

type AddingState = {
     Image: Bitmap
     ConsecutiveFailuresCount: int
}

let addWord (targetColors: Color[,]) (state:AddingState) (index, word) =
    printfn "Word %d: %s" index word

    let watch = System.Diagnostics.Stopwatch.StartNew()

    let drawnPixels = state.Image |> getColors 

    printfn "Elapsed time in seconds: %f" watch.Elapsed.TotalSeconds

    use g = state.Image |> Graphics.FromImage
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    let boundaries = getBoundaries targetColors drawnPixels

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
        for fontSize in fontSizes do
            use font = getFont fontSize
            let wordToDraw = word
            let size = g.MeasureString(wordToDraw, font)
            let width, height = int size.Width, int size.Height

            match findSpot (width, height) (0, 0) with
            | Some (x, y) ->
                yield {
                    X = x
                    Y = y
                    FontSize = fontSize
                    Width = width
                    Height = height
                }
            | None -> ()
        }

    let bestSpot = spots |> Seq.tryHead
         
    let consecutiveFailuresCount =
        match bestSpot with
        | Some spot ->
            printfn "Found a spot! %A, size %d" (spot.X, spot.Y) spot.FontSize
            let middle = targetColors.[spot.X + spot.Width / 2, spot.Y + spot.Height / 2]
            let colorToUse = middle //Color.White
            use font = getFont spot.FontSize
            use brush = new SolidBrush(colorToUse)
            g.DrawString(word, font, brush, float32 spot.X, float32 spot.Y)
            0
        | None ->
            printfn "No spot found :("
            state.ConsecutiveFailuresCount + 1

    printfn "Elapsed time in seconds: %f" watch.Elapsed.TotalSeconds

    {
        Image = state.Image
        ConsecutiveFailuresCount = consecutiveFailuresCount
    }

let addWords targetColors state words =
    let stateWithWordsAdded =
        words
        |> Seq.scan (addWord targetColors) state
        |> Seq.takeWhile (fun s -> s.ConsecutiveFailuresCount < 10)
        |> Seq.last
    { state with ConsecutiveFailuresCount = 0 }

let generate (inputFolder:string, outputFolder:string) (inputFile, wordSets) =

    let target = Bitmap.FromFile(inputFolder + inputFile) :?> Bitmap
    let targetColors = getColors target
    let empty = new Bitmap(target.Width, target.Height)

    let result =
        let startingState =
            {
                Image = empty
                ConsecutiveFailuresCount = 0
            }

        let wordsLayer = wordSets |> Seq.fold (addWords targetColors) startingState
        let result = new Bitmap(target.Width, target.Height)
        use g = Graphics.FromImage result
        g.DrawImage(wordsLayer.Image, 0.f, 0.f)
        result

    result.Save(outputFolder + inputFile, ImageFormat.Png)
