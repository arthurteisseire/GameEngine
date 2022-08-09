module ComponentAttack exposing (..)


type alias ComponentAttack =
    Maybe
        { x : Int
        , y : Int
        }


identity : ComponentAttack
identity =
    Nothing
