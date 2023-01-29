module ComponentAttack exposing (..)

import Vector2 exposing (Vector2)


type alias ComponentAttack =
    Maybe
        { from : Vector2 Float
        , to : Vector2 Float
        }


identity : ComponentAttack
identity =
    Nothing
