module ComponentVisual exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.Modifier as Modifier
import Svg exposing (Svg)
import Vector2 exposing (Vector2)


type VisualMsg
    = Clicked
    | None


type alias ComponentVisual =
    { shape : List (Svg.Attribute VisualMsg) -> List (Svg VisualMsg) -> Svg VisualMsg
    , attributes : List (Svg.Attribute VisualMsg)
    , posToAttributes : Vector2 Float -> List (Svg.Attribute VisualMsg)
    , position : Vector2 Float
    }


emptyTable : ComponentTable ComponentVisual
emptyTable =
    ComponentTable.empty
        { toString =
            \visualA ->
                "Visual(position=(" ++ Vector2.vectorFloatToString visualA.position ++ "))"
        }


modifier =
    Modifier.init
        { get = .visualComponents
        , set = \table world -> { world | visualComponents = table }
        }
