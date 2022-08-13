module ComponentAI exposing (..)


type alias ComponentAI =
    { remainingTurnsBeforeMove : Int
    }


identity : ComponentAI
identity =
    { remainingTurnsBeforeMove = 3
    }


toString : ComponentAI -> String
toString ai =
    "AI(remainingTurnsBeforeMove = "
        ++ String.fromInt ai.remainingTurnsBeforeMove
        ++ ")"
