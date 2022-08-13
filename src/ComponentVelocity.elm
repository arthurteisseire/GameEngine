module ComponentVelocity exposing (..)


type alias ComponentVelocity =
    { x : Int
    , y : Int
    }


identity : ComponentVelocity
identity =
    { x = 0
    , y = 0
    }


toString : ComponentVelocity -> String
toString velocity =
    "Velocity(x = "
        ++ String.fromInt velocity.x
        ++ ", y = "
        ++ String.fromInt velocity.y
        ++ ")"
