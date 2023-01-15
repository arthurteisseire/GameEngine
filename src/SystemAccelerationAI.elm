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
    updateComponentsWithOthers
        { func = updateAIVelocity
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .turnComponents
                |> withInput .velocityComponents
                |> withInput .positionComponents
        , otherComponents =
            select OtherComponents
                |> using .entities
                |> andFrom .playerComponents
                |> andFrom .positionComponents
        , output =
            toOutputComponents
                |> withOutput velocityComponent
        }
        entityId
        world


updateAIVelocity : Table OtherComponents -> InputComponents -> OutputComponents
updateAIVelocity inputTable { turn, velocity, position } =
    let
        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map .position (valuesTable inputTable)))

        diff =
            Vector2.sub playerPos position

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
