module FoxLogic

open System.Drawing

open TextCandidates
open ISpotFinder

type Boundaries = {
    Width: int
    Height: int
    AvailableRight: int [,]
    }

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

let countFalse repetitions = seq {
    for (start, count, value) in repetitions do
        if value then
            for i in count .. -1 .. 1 do yield -i
        else
            for i in count .. -1 .. 1 do yield i
    }

let updateBoundaries (forbiddenPixels:bool[,]) (minY, maxY) boundaries =
    let inline fill y start count value = 
        if value then                   
            for i in count .. -1 .. 1 do boundaries.AvailableRight.[start+count-i, y] <- -i
        else
            for i in count .. -1 .. 1 do boundaries.AvailableRight.[start+count-i, y] <- i

    for y in minY .. maxY do
        let mutable currentGroup : option<int * int * _>  = None
        for x in 0 .. boundaries.Width - 1 do
            let elem = forbiddenPixels.[x, y]
            match currentGroup with
            | None ->
                currentGroup <- Some (0, 1, elem)
            | Some (start, n, value) ->
                if value = elem then
                    currentGroup <- Some (start, n+1, value)
                else
                    fill y start n value
                    currentGroup <- Some (start+n, 1, elem)
                        
        match currentGroup with
        | None -> ()
        | Some (start, n, value) -> fill y start n value

    boundaries

let getBoundaries (forbiddenPixels:bool[,]) = 
    let width = forbiddenPixels.GetLength(0)
    let height = forbiddenPixels.GetLength(1)
    let availableRight = Array2D.zeroCreate width height
    {
        Width = width
        Height = height
        AvailableRight = availableRight
    } |> updateBoundaries forbiddenPixels (0, height - 1)

type AddingState = {
     ForbiddenPixels: bool[,]
     Boundaries: Boundaries
     WordsSpots: Spot list
     RemainingWords: (string * TextCandidate list) list
     NextIterationWords: (string * TextCandidate list) list
}

let findSpot boundaries (width, height) =
    let rec spotOk (x,y) remaining =
        match remaining with
        | 0 -> true, 0
        | remaining ->
            let available = boundaries.AvailableRight.[x, y]
            if available >= width then
                spotOk (x, y+1) (remaining-1)
            else
                false, available

    let mutable x = 0
    let mutable y = 0
    let mutable found = false
    while y < boundaries.Height - height + 1 && not found do
        let ok, moveBy = spotOk (x,y) height
        if ok then
            found <- true
        else
            x <- x + (abs moveBy)
            if x >= boundaries.Width then
                y <- y + 1
                x <- 0

    if found then
        Some (x,y)
    else
        None

let addWord (state:AddingState) =
    let word, textCandidates = state.RemainingWords.Head

    let totalCandidatesCount = (state.RemainingWords @ state.NextIterationWords) |> List.collect snd |> List.length
    
    printfn "%s, remaining candidates = %d" word totalCandidatesCount
    
    let boundaries = state.Boundaries

    let spots = seq {
        for textCandidate in textCandidates do
            match findSpot boundaries (textCandidate.Width, textCandidate.Height) with
            | Some (x, y) ->
                yield {
                    X = x
                    Y = y
                    TextCandidate = textCandidate
                }
            | None -> ()
        }
    //let sw = System.Diagnostics.Stopwatch.StartNew()
    let bestSpot = spots |> Seq.tryHead
    //printfn "Spot computation: %O" sw.Elapsed 

    let newState =
        match bestSpot with
        | Some spot ->
            for x in 0 .. spot.TextCandidate.Width - 1 do
                for y in 0 .. spot.TextCandidate.Height - 1 do
                    state.ForbiddenPixels.[spot.X + x, spot.Y + y] <- state.ForbiddenPixels.[spot.X + x, spot.Y + y] || spot.TextCandidate.Pixels.[x, y]

            let remainingCandidates = textCandidates |> List.skipWhile (fun c -> c <> spot.TextCandidate)
            //let w = System.Diagnostics.Stopwatch.StartNew()
            let updatedBoundaries = state.Boundaries |> updateBoundaries state.ForbiddenPixels (spot.Y, spot.Y + spot.TextCandidate.Height - 1)
            //printfn "Boundaries: %O" w.Elapsed 
            
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

let rec addWords shuffle (state:AddingState) =
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
        addWords shuffle state'
    | _ ->
        //let w = System.Diagnostics.Stopwatch.StartNew ()
        let state' = addWord state
        //printfn "State evol : %O" w.Elapsed
        addWords shuffle state'

let makeForbidenPixels (colors: Color[,]) =
    Array2D.init (colors.GetLength(0)) (colors.GetLength(1)) (fun x y -> colors.[x, y].A < 15uy)

let placeWords (targetColors: Color[,]) words shuffle = 
    let forbiddenPixels = makeForbidenPixels targetColors
    let startingState =
        {
            ForbiddenPixels = forbiddenPixels
            Boundaries = getBoundaries forbiddenPixels
            WordsSpots = []
            RemainingWords = []
            NextIterationWords = words
        }

    let finalState = addWords shuffle startingState
    finalState.WordsSpots

type FoxSpotFinder(shuffle:bool) =
    interface ISpotFinder with
        member x.FindSpots (targetColors: System.Drawing.Color[,]) (words: (string * TextCandidate list) list) =
            placeWords targetColors words shuffle