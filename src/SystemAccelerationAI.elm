module SystemAccelerationAI exposing (updateEntity)

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


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map3
            (\ai velocity position ->
                let
                    inputComponents =
                        (InputComponents
                            |> from world.playerComponents
                            |> join world.positionComponents
                        )
                            world.entities

                    components =
                        updateAIVelocity entityId inputComponents (OutputComponents ai velocity position)
                in
                { world
                    | aiComponents = insertComponent entityId components.ai world.aiComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                    , positionComponents = insertComponent entityId components.position world.positionComponents
                }
            )
            (getComponent entityId world.aiComponents)
            (getComponent entityId world.velocityComponents)
            (getComponent entityId world.positionComponents)


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
