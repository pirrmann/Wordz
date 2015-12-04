module Logic

open System.Drawing
open System.Drawing.Imaging
open System.IO
    
let getColors (bitmap:Bitmap) =
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

type TextCandidate = {
    Text: string
    FontSize: int
    Width: int
    Height: int
    Offset: int * int
    Pixels: bool[,]
}

let getFont size = new Font("Arial", float32 size)

let buildTestCandidates word fontSizes =
    use bit = new Bitmap(1, 1)
    use measureGraphics = bit |> Graphics.FromImage
    measureGraphics.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    let buildTestCandidate margin fontSize =
        use font = getFont fontSize
        let size = measureGraphics.MeasureString(word, font)
        let width, height = int size.Width, int size.Height
        use bitmap = new Bitmap(width, height)
        use g = bitmap |> Graphics.FromImage
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias
        g.DrawString(word, font, Brushes.Black, 0.f, 0.f)

        let pixels = Array2D.init width height (fun x y -> bitmap.GetPixel(x, y).A > 15uy)

        let keepMargin margin whiteSpace =
            max (whiteSpace - margin) 0

        let startOffsetX =
            [0 .. width - 1]
            |> Seq.takeWhile(fun x -> pixels.[x, 0 .. height - 1] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedEndColumnsCount =
            [0 .. width - 1]
            |> Seq.rev
            |> Seq.takeWhile(fun x -> pixels.[x, 0 .. height - 1] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let startOffsetY =
            [0 .. height - 1]
            |> Seq.takeWhile(fun y -> pixels.[0 .. width - 1, y] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedEndLinesCount =
            [0 .. height - 1]
            |> Seq.rev
            |> Seq.takeWhile(fun y -> pixels.[0 .. width - 1, y] |> Array.forall ((=) false))
            |> Seq.length
            |> keepMargin margin

        let trimmedPixels =
            pixels.[startOffsetX .. width - 1- trimmedEndColumnsCount, startOffsetY .. height - 1 - trimmedEndLinesCount]

        {
            Text = word
            FontSize = fontSize
            Width = width - startOffsetX - trimmedEndColumnsCount
            Height = height - startOffsetY - trimmedEndLinesCount
            Offset = startOffsetX, startOffsetY
            Pixels = trimmedPixels
        }

    fontSizes |> List.map (buildTestCandidate 1)
    
type Spot = {
    X:int
    Y:int
    TextCandidate: TextCandidate
}

type Boundaries = {
    Width: int
    Height: int
    NextInvalidRight: int[,]
    NextFreeRight: int[,] }

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

let indexesOfNextTrueAndFalse repetitions = seq {
    for (start, count, value) in repetitions do
        for i in start..start+count-1 do
            if value then
                yield (i, start+count)
            else
                yield (start+count, i)
    }

let updateBoundaries (forbiddenPixels:bool[,]) ((minX, maxX), (minY, maxY)) boundaries =
    [|
        for y in minY .. maxY do
            yield async {
                let nextTrueAndFalseOnLine =
                    seq { for x in 0 .. boundaries.Width - 1 do yield forbiddenPixels.[x, y] }
                    |> groupConsecutive
                    |> indexesOfNextTrueAndFalse
                    |> Seq.mapi (fun i value -> i, value)

                for x, (nextTrue, nextFalse) in nextTrueAndFalseOnLine do
                    boundaries.NextFreeRight.[x,y] <- nextFalse
                    boundaries.NextInvalidRight.[x,y] <- nextTrue
            }
    |]
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    boundaries

let getBoundaries (forbiddenPixels:bool[,]) = 
    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)

    let nextFreeRight = Array2D.zeroCreate width height
    let nextInvalidRight = Array2D.zeroCreate width height

    {
        Width = width
        Height = height
        NextFreeRight = nextFreeRight
        NextInvalidRight = nextInvalidRight
    } |> updateBoundaries forbiddenPixels ((0, width - 1), (0, height - 1))

type FitResult =
    | Fits
    | CannotFitUntilX of int

let canFit boundaries (width, height) (x, y)  =
    if y + height > boundaries.Height then
        CannotFitUntilX boundaries.Width
    else
        let blockingPixelsRight =
            [| y .. min (y + height) (boundaries.Height - 1) |]
            |> Array.map (fun y -> boundaries.NextInvalidRight.[x, y], y)
            |> Array.filter (fun (right, _) -> right < x + width)
    
        if blockingPixelsRight.Length > 0 then
            let blockingX, blockingY = Array.maxBy fst blockingPixelsRight
            let nextFreeX = if blockingX < boundaries.NextFreeRight.GetLength(0) && blockingY < boundaries.NextFreeRight.GetLength(1)
                            then boundaries.NextFreeRight.[blockingX, blockingY]
                            else blockingX
            CannotFitUntilX nextFreeX
        else
            Fits

type AddingState = {
     ForbiddenPixels: bool[,]
     Boundaries: Boundaries
     WordsSpots: Spot list
     RemainingWords: (string * TextCandidate list) list
     NextIterationWords: (string * TextCandidate list) list
}

let addWord (targetColors: Color[,]) (state:AddingState) =
    let word, textCandidates = state.RemainingWords.Head

    let totalCandidatesCount = (state.RemainingWords @ state.NextIterationWords) |> List.collect snd |> List.length
    
    printfn "%s, remaining candidates = %d" word totalCandidatesCount
    
    let boundaries = state.Boundaries

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
            for x in 0 .. spot.TextCandidate.Width - 1 do
                for y in 0 .. spot.TextCandidate.Height - 1 do
                    state.ForbiddenPixels.[spot.X + x, spot.Y + y] <- state.ForbiddenPixels.[spot.X + x, spot.Y + y] || spot.TextCandidate.Pixels.[x, y]

            let remainingCandidates = textCandidates |> List.skipWhile (fun c -> c <> spot.TextCandidate)
            let updatedBoundaries = state.Boundaries |> updateBoundaries state.ForbiddenPixels ((spot.X, spot.X + spot.TextCandidate.Width - 1), (spot.Y, spot.Y + spot.TextCandidate.Height - 1))

            {
                ForbiddenPixels = state.ForbiddenPixels
                Boundaries = updatedBoundaries
                WordsSpots = spot :: state.WordsSpots
                RemainingWords = state.RemainingWords.Tail
                NextIterationWords = (word, remainingCandidates) :: state.NextIterationWords
            }

        | None ->
            { state with RemainingWords = state.RemainingWords.Tail }

    newState

let rand =
    let r = new System.Random(42)
    fun () -> r.NextDouble()

let rec addWords shuffle targetColors (state:AddingState) =
    match state.RemainingWords, state.NextIterationWords with
    | [], [] -> state
    | [], nextIterationWords ->
        let remainingWords =
            if shuffle then
                nextIterationWords |> List.sortBy (fun _ -> rand())
            else
                nextIterationWords |> List.rev

        let state' = { state with RemainingWords = remainingWords
                                  NextIterationWords = [] }
        addWords shuffle targetColors state'
    | _ ->
        let state' = addWord targetColors state
        addWords shuffle targetColors state'

let generate (inputFolder:string, outputFolder:string) shuffle (inputFile, words) =
    let path = Path.Combine(inputFolder, inputFile)
    let target = Bitmap.FromFile(path) :?> Bitmap
    let targetColors = getColors target
    let empty = new Bitmap(target.Width, target.Height)
    let result =
        let forbiddenPixels = Array2D.init target.Width target.Height (fun x y -> targetColors.[x, y].A < 15uy)
        let startingState =
            {
                ForbiddenPixels = forbiddenPixels
                Boundaries = getBoundaries forbiddenPixels
                WordsSpots = []
                RemainingWords = []
                NextIterationWords = words
            }

        let finalState = addWords shuffle targetColors startingState

        let result = new Bitmap(target.Width, target.Height)
        use g = Graphics.FromImage result
        g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

        for spot in finalState.WordsSpots do
            let color = targetColors.[spot.X + spot.TextCandidate.Width / 2, spot.Y + spot.TextCandidate.Height / 2]
            use font = getFont spot.TextCandidate.FontSize
            use brush = new SolidBrush(color)
            g.DrawString(spot.TextCandidate.Text, font, brush, single (spot.X - fst spot.TextCandidate.Offset), single (spot.Y - snd spot.TextCandidate.Offset))

        result
        
    let outputPath = Path.Combine(outputFolder, inputFile)
    Directory.CreateDirectory(outputFolder) |> ignore
    result.Save(outputPath, ImageFormat.Png)
