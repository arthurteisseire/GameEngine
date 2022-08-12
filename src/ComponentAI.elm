module ComponentAI exposing (..)


type alias ComponentAI =
    { remainingTurnsBeforeMove : Int
    }


identity : ComponentAI
identity =
    { remainingTurnsBeforeMove = 3
    }
