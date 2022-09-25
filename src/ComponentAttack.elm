module ComponentAttack exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentAttack =
    Maybe
        { from : Vector2
        , to : Vector2
        }


identity : ComponentAttack
identity =
    Nothing


toString : ComponentAttack -> String
toString maybeAttack =
    case maybeAttack of
        Just attack ->
            "Attack(from="
                ++ Vector2.toString attack.from
                ++ ", to="
                ++ Vector2.toString attack.to
                ++ ")"

        Nothing ->
            "Attack()"
