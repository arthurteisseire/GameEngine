module SystemAccelerationAI exposing (..)

import ComponentAI exposing (ComponentAI)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import World exposing (World)


type alias InputComponents =
    { player : ComponentPlayer
    , position : ComponentPosition
    }


type alias OutputComponents =
    { ai : ComponentAI
    , velocity : ComponentVelocity
    , position : ComponentPosition
    }


updateWorld : World -> World
updateWorld world =
    updateEntities
        updateAIVelocity
        (\entityId { ai, velocity, position } accWorld ->
            { accWorld
                | aiComponents = setComponent entityId ai accWorld.aiComponents
                , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                , positionComponents = setComponent entityId position accWorld.positionComponents
            }
        )
        world.entities
        (intersectTable2 InputComponents world.entities world.playerComponents world.positionComponents)
        (intersectTable3 OutputComponents world.entities world.aiComponents world.velocityComponents world.positionComponents)
        world


updateAIVelocity : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
updateAIVelocity _ inputTable { ai, velocity, position } =
    let
        remainingTurns =
            ai.remainingTurnsBeforeMove - 1

        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map .position (valuesTable inputTable)))

        diff =
            { x = playerPos.x - position.x, y = playerPos.y - position.y }

        nextVelocity =
            if abs diff.x > abs diff.y then
                { x = diff.x // abs diff.x, y = 0 }

            else
                { x = 0, y = diff.y // abs diff.y }
    in
    if remainingTurns < 0 then
        { ai = { ai | remainingTurnsBeforeMove = 4 }
        , velocity = nextVelocity
        , position = position
        }

    else
        { ai = { ai | remainingTurnsBeforeMove = remainingTurns }
        , velocity = velocity
        , position = position
        }
