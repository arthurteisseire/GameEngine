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
            0.05
    in
    Just
        { duration = duration
        , timeLeft = duration
        , offset = offset
        }


toString : ComponentAnimation -> String
toString maybeAnimation =
    let
        content =
            case maybeAnimation of
                Just animation ->
                    "duration="
                        ++ String.fromFloat animation.duration
                        ++ ", timeLeft="
                        ++ String.fromFloat animation.timeLeft
                        ++ ", offset="
                        ++ Vector2.vectorFloatToString animation.offset

                Nothing ->
                    ""
    in
    "ComponentAnimation(" ++ content ++ ")"
