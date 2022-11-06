module ComponentPosition exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentPosition =
    { previousPos : Vector2 Float
    , currentPos : Vector2 Float
    }


identity : ComponentPosition
identity =
    init Vector2.identity


init : Vector2 Float -> ComponentPosition
init vec =
    { previousPos = vec
    , currentPos = vec
    }


toString : ComponentPosition -> String
toString position =
    "Position(currentPos="
        ++ Vector2.vectorFloatToString position.currentPos
        ++ ", previousPos="
        ++ Vector2.vectorFloatToString position.previousPos
        ++ ")"
