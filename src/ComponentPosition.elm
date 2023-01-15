module ComponentPosition exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentPosition =
    Vector2 Float


identity : ComponentPosition
identity =
    Vector2.identity


toString : ComponentPosition -> String
toString position =
    "Position(" ++ Vector2.vectorFloatToString position ++ ")"
