module ComponentTerrain exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Vector2 exposing (Vector2)


type alias ComponentTerrain =
    { dimensions : Vector2 Int
    , sizeFactor : Int
    }


emptyTable : ComponentTable ComponentTerrain
emptyTable =
    ComponentTable.empty
        { toString =
            \terrainA ->
                "Terrain(dimensions="
                    ++ Vector2.toString String.fromInt terrainA.dimensions
                    ++ ", "
                    ++ "sizeFactor="
                    ++ String.fromInt terrainA.sizeFactor
                    ++ ")"
        }


modifier =
    Modifier.init
        { get = .terrainComponents
        , set = \table world -> { world | terrainComponents = table }
        }
