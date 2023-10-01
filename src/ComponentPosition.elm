module ComponentPosition exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Vector2 exposing (Vector2)


type alias ComponentPosition =
    Vector2 Float


emptyTable : ComponentTable ComponentPosition
emptyTable =
    ComponentTable.empty
        { toString = \positionA -> "Position(" ++ Vector2.vectorFloatToString positionA ++ ")"
        }


modifier =
    Modifier.tableModifier
        { get = .positionComponents
        , set = \table context -> { context | positionComponents = table }
        }


identity : ComponentPosition
identity =
    Vector2.identity
