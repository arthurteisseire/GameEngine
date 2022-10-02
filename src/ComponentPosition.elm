module ComponentPosition exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentPosition =
    { previousPos : Vector2
    , currentPos : Vector2
    }


identity : ComponentPosition
identity =
    init Vector2.identity


init : Vector2 -> ComponentPosition
init vec =
    { previousPos = vec
    , currentPos = vec
    }


toString : ComponentPosition -> String
toString position =
    "Position(currentPos="
        ++ Vector2.toString position.currentPos
        ++ ", previousPos="
        ++ Vector2.toString position.previousPos
        ++ ")"
