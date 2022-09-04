module ComponentVisual exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import Vector2 exposing (Vector2)


type Msg
    = Clicked


type alias ComponentVisual =
    { shape : List (Svg.Attribute Msg) -> List (Svg Msg) -> Svg Msg
    , attributes : List (Svg.Attribute Msg)
    , posToAttributes : Vector2 -> List (Svg.Attribute Msg)
    }


toString : ComponentVisual -> String
toString _ =
    "Visual()"


defaultRect : ComponentVisual
defaultRect =
    { shape = Svg.rect
    , attributes =
        [ SA.width "1"
        , SA.height "1"
        , SA.fill "blue"
        , SE.onClick Clicked
        ]
    , posToAttributes =
        \v ->
            [ SA.x <| String.fromFloat v.x
            , SA.y <| String.fromFloat v.y
            ]
    }


defaultCircle : ComponentVisual
defaultCircle =
    { shape = Svg.circle
    , attributes =
        [ SA.r "0.5"
        , SA.fill "red"
        , SE.onClick Clicked
        ]
    , posToAttributes =
        \v ->
            [ SA.cx <| String.fromFloat (v.x + 0.5)
            , SA.cy <| String.fromFloat (v.y + 0.5)
            ]
    }
