module SystemAITurn exposing (..)

import ComponentAI exposing (ComponentAI)
import EntityTable exposing (..)
import World exposing (World)


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map
            (\ai ->
                { world | aiComponents = updateComponent entityId (playTurn entityId ai) world.aiComponents }
            )
            (getComponent entityId world.aiComponents)


playTurn : EntityId -> ComponentAI -> ComponentAI
playTurn _ ai =
    { ai
        | remainingTurnsBeforeMove = ai.remainingTurnsBeforeMove - 1
    }
