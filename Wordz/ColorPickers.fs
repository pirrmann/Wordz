module ColorPickers

let CentralColor (targetColors:System.Drawing.Color[,]) (spot:ISpotFinder.Spot) =
    let x = spot.X + spot.TextCandidate.Width / 2
    let y = spot.Y + spot.TextCandidate.Height / 2
    targetColors.[x, y]

let AverageRgbColor (targetColors:System.Drawing.Color[,]) (spot:ISpotFinder.Spot) =
    let avgR, avgG, avgB =
        let rSum, gSum, bSum =
            seq {
                for x in spot.X .. spot.X + spot.TextCandidate.Width - 1 do
                for y in spot.Y .. spot.Y + spot.TextCandidate.Height - 1 do
                let color = targetColors.[x, y]
                yield float color.R, float color.G, float color.B }
            |> Seq.reduce (fun (r, g, b) (r', g', b') -> r + r', g + g', b + b')

        let avg sum = sum / float (spot.TextCandidate.Width * spot.TextCandidate.Height) |> round |> int
        avg rSum, avg gSum, avg bSum 

    System.Drawing.Color.FromArgb(avgR, avgG, avgB)

let AverageRgbSquaredColor (targetColors:System.Drawing.Color[,]) (spot:ISpotFinder.Spot) =
    let avgR, avgG, avgB =
        let rSum, gSum, bSum =
            seq {
                for x in spot.X .. spot.X + spot.TextCandidate.Width - 1 do
                for y in spot.Y .. spot.Y + spot.TextCandidate.Height - 1 do
                let color = targetColors.[x, y]
                yield pown (float color.R) 2, pown (float color.G) 2, pown (float color.B) 2 }
            |> Seq.reduce (fun (r, g, b) (r', g', b') -> r + r', g + g', b + b')

        let avg sum = sum / float (spot.TextCandidate.Width * spot.TextCandidate.Height) |> sqrt |> round |> int
        avg rSum, avg gSum, avg bSum

    System.Drawing.Color.FromArgb(avgR, avgG, avgB)
