module TextCandidates

open System.Drawing

type TextCandidate = {
    Text: string
    FontSize: int
    Width: int
    Height: int
    Offset: int * int
    Pixels: bool[,]
}

let buildTestCandidates word fontSizes =
    use bit = new Bitmap(1, 1)
    use measureGraphics = bit |> Graphics.FromImage
    measureGraphics.TextRenderingHint <- Text.TextRenderingHint.AntiAlias

    let buildTestCandidate margin fontSize =
        use font = DrawingHelpers.getFont fontSize
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