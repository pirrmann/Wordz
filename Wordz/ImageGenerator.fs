module ImageGenerator

open System.IO
open System.Drawing
open DrawingHelpers

open ISpotFinder

let generate (spotFinder:ISpotFinder) (inputFolder:string, outputFolder:string) (inputFile, words) =
    let path = Path.Combine(inputFolder, inputFile)
    let target = Bitmap.FromFile(path) :?> Bitmap
    let targetColors = getColors target

    let wordsSpots = spotFinder.FindSpots targetColors words

    let result = new Bitmap(target.Width, target.Height)
    use g = Graphics.FromImage result
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    for spot in wordsSpots do
        let color = targetColors.[spot.X + spot.TextCandidate.Width / 2, spot.Y + spot.TextCandidate.Height / 2]
        use font = getFont spot.TextCandidate.FontSize
        use brush = new SolidBrush(color)
        g.DrawString(spot.TextCandidate.Text, font, brush, single (spot.X - fst spot.TextCandidate.Offset), single (spot.Y - snd spot.TextCandidate.Offset))
        
    let outputPath = Path.Combine(outputFolder, inputFile)
    Directory.CreateDirectory(outputFolder) |> ignore
    result.Save(outputPath, System.Drawing.Imaging.ImageFormat.Png)
