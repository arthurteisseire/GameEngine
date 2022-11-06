module ComponentVelocity exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentVelocity =
    Vector2 Float


identity : ComponentVelocity
identity =
    { x = 0
    , y = 0
    }


toString : ComponentVelocity -> String
toString velocity =
    "Velocity(x = "
        ++ String.fromFloat velocity.x
        ++ ", y = "
        ++ String.fromFloat velocity.y
        ++ ")"
