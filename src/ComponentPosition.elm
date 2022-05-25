module ComponentPosition exposing (ComponentPosition, identity, mapX)


type alias ComponentPosition =
    { x : Int
    , y : Int
    }


identity : ComponentPosition
identity =
    { x = 0
    , y = 0
    }


mapX : (Int -> Int) -> ComponentPosition -> ComponentPosition
mapX f position =
    { position | x = f position.x }
