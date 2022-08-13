module ComponentVisual exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE


type Msg
    = Clicked


type alias ComponentVisual =
    { shape : List (Svg.Attribute Msg) -> List (Svg Msg) -> Svg Msg
    , attributes : List (Svg.Attribute Msg)
    , posToAttributes : Int -> Int -> List (Svg.Attribute Msg)
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
        \x y ->
            [ SA.x <| String.fromInt x
            , SA.y <| String.fromInt y
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
        \x y ->
            [ SA.cx <| String.fromFloat (toFloat x + 0.5)
            , SA.cy <| String.fromFloat (toFloat y + 0.5)
            ]
    }
