module SystemAccelerationAI exposing (updateEntity)

import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


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
    update3Components
        (updateAIVelocity
            (select InputComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.playerComponents
                |> andFrom world.positionComponents
            )
        )
        OutputComponents
        turnComponent
        velocityComponent
        positionComponent
        entityId
        world


updateAIVelocity : Table InputComponents -> EntityId -> OutputComponents -> OutputComponents
updateAIVelocity inputTable _ { turn, velocity, position } =
    let
        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map .position (valuesTable inputTable)))

        diff =
            Vector2.sub playerPos.currentPos position.currentPos

        nextVelocity =
            if abs diff.x > abs diff.y then
                if diff.x == 0 then
                    Vector2.identity

                else
                    { x = diff.x / abs diff.x, y = 0 }

            else if diff.y == 0 then
                Vector2.identity

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
