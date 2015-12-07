module ISpotFinder

open TextCandidates

type Spot = {
    X:int
    Y:int
    TextCandidate: TextCandidate
}

type ISpotFinder =
    abstract member FindSpots: System.Drawing.Color[,] -> (string * TextCandidate list) list -> Spot list
