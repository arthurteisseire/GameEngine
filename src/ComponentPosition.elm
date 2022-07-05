module ComponentPosition exposing (ComponentPosition, identity)


type alias ComponentPosition =
    { x : Int
    , y : Int
    }


identity : ComponentPosition
identity =
    { x = 0
    , y = 0
    }
