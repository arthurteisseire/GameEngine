module SystemAccelerationAI exposing (updateEntity)

import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
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
updateEntity =
    Db.updateComponentsWithOthers
        { func = updateAIVelocity
        , inputComponents =
            Component.select InputComponents
                |> Component.join .turnComponents
                |> Component.join .velocityComponents
                |> Component.join .positionComponents
        , otherComponents =
            Db.select OtherComponents
                |> Db.fromEntities .entities
                |> Db.innerJoin .playerComponents
                |> Db.innerJoin .positionComponents
        , output =
            Modifier.select
                |> Modifier.join ( velocityModifier, .velocity )
        }


updateAIVelocity : Table OtherComponents -> InputComponents -> OutputComponents
updateAIVelocity inputTable { turn, velocity, position } =
    let
        playerPos =
            Maybe.withDefault ComponentPosition.identity (List.head (List.map .position (Table.values inputTable)))

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
