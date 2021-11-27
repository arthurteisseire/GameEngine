module ComponentLife exposing (ComponentLife, identity, mapHp)


type ComponentLife
    = ComponentLife Internals


type alias Internals =
    { healPoints : Int
    }


identity : ComponentLife
identity = ComponentLife { healPoints = 0 }


mapHp : (Int -> Int) -> ComponentLife -> ComponentLife
mapHp f (ComponentLife internals) =
    ComponentLife <| { internals | healPoints = f internals.healPoints }
