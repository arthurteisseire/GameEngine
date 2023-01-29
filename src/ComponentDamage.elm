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


