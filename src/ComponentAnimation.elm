module ComponentAnimation exposing (..)


type alias ComponentAnimation =
    Maybe
        { offsetX : Float
        , offsetY : Float
        }


identity : ComponentAnimation
identity =
    Nothing
