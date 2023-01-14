module SystemAccelerationAI exposing (updateEntity)

import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias OutputComponents =
    { velocity : ComponentVelocity
    }


type alias InputComponents =
    { turn : ComponentTurn
    , velocity : ComponentVelocity
    , position : ComponentPosition
    }


type alias OtherComponents =
    { player : ComponentPlayer
    , position : ComponentPosition
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    updateComponentsWithOthersNew
        { db = world
        , entityId = entityId
        , func = updateAIVelocity entityId
        , inputComponents =
            Just InputComponents
                |> withComponent entityId world.turnComponents
                |> withComponent entityId world.velocityComponents
                |> withComponent entityId world.positionComponents
        , otherComponents =
            select OtherComponents
                |> using world.entities
                |> remove entityId
                |> andFrom world.playerComponents
                |> andFrom world.positionComponents
        , output =
            update1ComponentNew
                velocityComponent
        }


updateAIVelocity : EntityId -> Table OtherComponents -> InputComponents -> OutputComponents
updateAIVelocity _ inputTable { turn, velocity, position } =
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
        { velocity = nextVelocity
        }

    else
        { velocity = velocity
        }
