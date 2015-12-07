module DrawingHelpers

open System.Drawing

let getColors (bitmap:Bitmap) =
    Array2D.init bitmap.Width bitmap.Height (fun x y -> bitmap.GetPixel(x, y))

let getFont size = new Font("Arial", float32 size)
