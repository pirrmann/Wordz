module ImageGenerator

open System.IO
open System.Drawing
open DrawingHelpers

open ISpotFinder

let generate (spotFinder:ISpotFinder) (colorPicker:Color[,]->Spot->Color) (inputFilePath:string, outputFilePath:string) words =
    let target = Bitmap.FromFile(inputFilePath) :?> Bitmap
    let targetColors = getColors target

    let wordsSpots = spotFinder.FindSpots targetColors words

    let result = new Bitmap(target.Width, target.Height)
    use g = Graphics.FromImage result
    g.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    for spot in wordsSpots do
        use font = getFont spot.TextCandidate.FontSize
        let color = colorPicker targetColors spot
        use brush = new SolidBrush(color)
        g.DrawString(spot.TextCandidate.Text, font, brush, single (spot.X - fst spot.TextCandidate.Offset), single (spot.Y - snd spot.TextCandidate.Offset))
        
    result.Save(outputFilePath, System.Drawing.Imaging.ImageFormat.Png)
