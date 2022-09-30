module ComponentTurn exposing (..)


type alias ComponentTurn =
    { turnsToPlay : Int
    , remainingTurns : Int
    }


identity : ComponentTurn
identity =
    let
        turns =
            0
    in
    { turnsToPlay = turns
    , remainingTurns = turns
    }


toString : ComponentTurn -> String
toString turn =
    "ComponentTurn(remainingTurns=" ++ String.fromInt turn.remainingTurns ++ ")"