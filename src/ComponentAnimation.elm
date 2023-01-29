module ComponentAnimation exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentAnimation =
    Maybe
        { duration : Float
        , timeLeft : Float
        , offset : Vector2 Float
        }


identity : ComponentAnimation
identity =
    Nothing


attackAnimation : Vector2 Float -> ComponentAnimation
attackAnimation offset =
    let
        duration =
            0.1
    in
    Just
        { duration = duration
        , timeLeft = duration
        , offset = offset
        }
