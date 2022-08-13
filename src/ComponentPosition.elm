module ComponentPosition exposing (..)


type alias ComponentPosition =
    { x : Int
    , y : Int
    }


identity : ComponentPosition
identity =
    { x = 0
    , y = 0
    }


toString : ComponentPosition -> String
toString position =
    "Position(x = "
        ++ String.fromInt position.x
        ++ ", y = "
        ++ String.fromInt position.y
        ++ ")"
