module SystemAccelerationAI exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntity2
                updatePlayerVelocity
                world.entities
                world.aiComponents
                world.velocityComponents
    in
    { world
        | aiComponents = tables.first
        , velocityComponents = tables.second
    }


updatePlayerVelocity : EntityId -> ComponentAI -> ComponentVelocity -> Tuple2 ComponentAI ComponentVelocity
updatePlayerVelocity _ ai velocity =
    let
        remainingTurns =
            ai.remainingTurnsBeforeMove - 1
    in
    if remainingTurns == 0 then
        toTuple2 { ai | remainingTurnsBeforeMove = 4 } { x = 0, y = 1 }

    else
        toTuple2 { ai | remainingTurnsBeforeMove = remainingTurns } velocity
