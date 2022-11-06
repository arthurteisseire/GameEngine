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
    , posToAttributes : Vector2 Float -> List (Svg.Attribute Msg)
    , position : Vector2 Float
    }


toString : ComponentVisual -> String
toString visual =
    "Visual(position=(" ++ Vector2.vectorFloatToString visual.position ++ "))"


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
    , position = { x = 0, y = 0 }
    }


circle : String -> ComponentVisual
circle color =
    { shape = Svg.circle
    , attributes =
        [ SA.r "0.5"
        , SA.fill color
        , SE.onClick Clicked
        ]
    , posToAttributes =
        \v ->
            [ SA.cx <| String.fromFloat (v.x + 0.5)
            , SA.cy <| String.fromFloat (v.y + 0.5)
            ]
    , position = { x = 0, y = 0 }
    }


terrain : ComponentVisual
terrain =
    { shape = Svg.rect
    , attributes =
        [ SA.width "100%"
        , SA.height "100%"
        , SA.fill "none"
        , SA.stroke "black"
        , SA.strokeWidth "0.05"
        , SE.onClick Clicked
        ]
    , posToAttributes =
        \v ->
            [ SA.x <| String.fromFloat v.x
            , SA.y <| String.fromFloat v.y
            ]
    , position = { x = 0, y = 0 }
    }
