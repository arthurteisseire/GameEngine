module ComponentLife exposing (..)


type alias ComponentLife =
    { healPoints : Int
    }


identity : ComponentLife
identity =
    { healPoints = 0
    }
