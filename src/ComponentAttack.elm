module ComponentAttack exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentAttack =
    Maybe Vector2


identity : ComponentAttack
identity =
    Nothing


toString : ComponentAttack -> String
toString maybeAttack =
    case maybeAttack of
        Just attack ->
            "Attack(x = "
                ++ String.fromFloat attack.x
                ++ ", y = "
                ++ String.fromFloat attack.y
                ++ ")"

        Nothing ->
            "Attack()"
