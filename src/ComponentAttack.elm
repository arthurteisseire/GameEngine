module ComponentAttack exposing (..)


type alias ComponentAttack =
    Maybe
        { x : Int
        , y : Int
        }


identity : ComponentAttack
identity =
    Nothing


toString : ComponentAttack -> String
toString maybeAttack =
    case maybeAttack of
        Just attack ->
            "Attack(x = "
                ++ String.fromInt attack.x
                ++ ", y = "
                ++ String.fromInt attack.y
                ++ ")"

        Nothing ->
            ""
