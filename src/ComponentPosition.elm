module ComponentPosition exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentPosition =
    Vector2


identity : ComponentPosition
identity =
    { x = 0
    , y = 0
    }


toString : ComponentPosition -> String
toString position =
    "Position(x = "
        ++ String.fromFloat position.x
        ++ ", y = "
        ++ String.fromFloat position.y
        ++ ")"
