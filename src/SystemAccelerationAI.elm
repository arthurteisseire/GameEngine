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
    updateEntitiesWithOthers
        { updateComponents = updateAIVelocity
        , updateWorld =
            \entityId { ai, velocity, position } accWorld ->
                { accWorld
                    | aiComponents = setComponent entityId ai accWorld.aiComponents
                    , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                    , positionComponents = setComponent entityId position accWorld.positionComponents
                }
        , world = world
        , entityTable = world.entities
        , readTable =
            InputComponents
                |> from world.playerComponents
                |> join world.positionComponents
        , writeTable =
            OutputComponents
                |> from world.aiComponents
                |> join world.velocityComponents
                |> join world.positionComponents
        }


updateAIVelocity : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
updateAIVelocity _ inputTable { ai, velocity, position } =
    let
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
    if ai.remainingTurnsBeforeMove > 0 then
        { ai = { ai | remainingTurnsBeforeMove = ai.remainingTurnsBeforeMove - 1 }
        , velocity = velocity
        , position = position
        }

    else
        { ai = { ai | remainingTurnsBeforeMove = 4 }
        , velocity = nextVelocity
        , position = position
        }
