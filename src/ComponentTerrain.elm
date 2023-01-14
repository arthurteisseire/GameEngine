module ComponentTerrain exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentTerrain =
    { dimensions : Vector2 Int
    , sizeFactor : Int
    }


toString : ComponentTerrain -> String
toString terrain =
    "Terrain(dimensions="
        ++ Vector2.toString String.fromInt terrain.dimensions
        ++ ", "
        ++ "sizeFactor="
        ++ String.fromInt terrain.sizeFactor
        ++ ")"
