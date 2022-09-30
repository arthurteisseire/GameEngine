module SystemAccelerationAI exposing (updateEntity)

import ComponentTurn exposing (ComponentTurn)
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
    { turn : ComponentTurn
    , velocity : ComponentVelocity
    , position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map3
            (\turn velocity position ->
                let
                    inputComponents =
                        (InputComponents
                            |> from world.playerComponents
                            |> join world.positionComponents
                        )
                            world.entities

                    components =
                        updateAIVelocity entityId inputComponents (OutputComponents turn velocity position)
                in
                { world
                    | turnComponents = insertComponent entityId components.turn world.turnComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                    , positionComponents = insertComponent entityId components.position world.positionComponents
                }
            )
            (getComponent entityId world.turnComponents)
            (getComponent entityId world.velocityComponents)
            (getComponent entityId world.positionComponents)


updateAIVelocity : EntityId -> Table InputComponents -> OutputComponents -> OutputComponents
updateAIVelocity _ inputTable { turn, velocity, position } =
    let
        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map .position (valuesTable inputTable)))

        diff =
            { x = playerPos.x - position.x, y = playerPos.y - position.y }

        nextVelocity =
            if abs diff.x > abs diff.y then
                { x = diff.x / abs diff.x, y = 0 }

            else
                { x = 0, y = diff.y / abs diff.y }
    in
    if turn.remainingTurns <= 0 then
        { turn = turn
        , velocity = nextVelocity
        , position = position
        }

    else
        { turn = turn
        , velocity = velocity
        , position = position
        }
