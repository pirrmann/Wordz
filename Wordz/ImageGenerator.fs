module ImageGenerator

open System.IO
open System.Drawing
open DrawingHelpers

open ISpotFinder

let generate (spotFinder:ISpotFinder) (inputFilePath:string, outputFilePath:string) words =
    let target = Bitmap.FromFile(inputFilePath) :?> Bitmap
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
        
    result.Save(outputFilePath, System.Drawing.Imaging.ImageFormat.Png)
