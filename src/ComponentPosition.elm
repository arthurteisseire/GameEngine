module ComponentPosition exposing (ComponentPosition, identity, getX, getY, mapX)


type ComponentPosition
    = ComponentPosition Internals


type alias Internals =
    { x : Int
    , y : Int
    }


identity : ComponentPosition
identity =
    ComponentPosition
        { x = 0
        , y = 0
        }

mapX : (Int -> Int) -> ComponentPosition -> ComponentPosition
mapX f (ComponentPosition internals) =
    ComponentPosition { internals | x = f internals.x }


getX : ComponentPosition -> Int
getX (ComponentPosition internals) =
    internals.x


getY : ComponentPosition -> Int
getY (ComponentPosition internals) =
    internals.y
