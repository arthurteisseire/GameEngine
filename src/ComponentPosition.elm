module ComponentPosition exposing (ComponentPosition, identity, mapX, mapY)


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


mapY : (Int -> Int) -> ComponentPosition -> ComponentPosition
mapY f position =
    { position | y = f position.y }
