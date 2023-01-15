module ComponentPosition exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentPosition =
    Vector2 Float


identity : ComponentPosition
identity =
    init Vector2.identity


init : Vector2 Float -> ComponentPosition
init vec =
    vec


toString : ComponentPosition -> String
toString position =
    "Position(" ++ Vector2.vectorFloatToString position ++ ")"
