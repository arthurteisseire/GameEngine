module ComponentLife exposing (..)


type alias ComponentLife =
    { healPoints : Int
    }


identity : ComponentLife
identity =
    { healPoints = 0
    }


toString : ComponentLife -> String
toString life =
    "Life(healPoints = "
        ++ String.fromInt life.healPoints
        ++ ")"
