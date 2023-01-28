module ComponentDamage exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentDamage =
    List
        { fromDirection : Vector2 Float
        , points : Int
        }


identity : ComponentDamage
identity =
    []


toString : ComponentDamage -> String
toString damage =
    "ComponentDamage("
        ++ List.foldl
            (\dam finalString ->
                finalString
                    ++ "fromDirection="
                    ++ Vector2.vectorFloatToString dam.fromDirection
                    ++ ", points="
                    ++ String.fromInt dam.points
            )
            ""
            damage
        ++ ")"
