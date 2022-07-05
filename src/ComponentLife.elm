module ComponentLife exposing (ComponentLife, identity)


type alias ComponentLife =
    { healPoints : Int
    }


identity : ComponentLife
identity =
    { healPoints = 0
    }
