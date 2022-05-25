module ComponentLife exposing (ComponentLife, identity, mapHp)


type alias ComponentLife =
    { healPoints : Int
    }


identity : ComponentLife
identity =
    { healPoints = 0 }


mapHp : (Int -> Int) -> ComponentLife -> ComponentLife
mapHp f life =
    { healPoints = f life.healPoints }
