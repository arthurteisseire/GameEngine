module SystemAccelerationAI exposing (updateEntity)

import ComponentAI exposing (ComponentAI)
import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import ComponentTurn exposing (ComponentTurn)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Vector2


type alias OutputComponents =
    { velocity : ComponentVelocity
    }


type alias InputComponents =
    { turn : ComponentTurn
    , velocity : ComponentVelocity
    , position : ComponentPosition
    , ai : ComponentAI
    }


type alias OtherComponents =
    { player : ComponentPlayer
    , position : ComponentPosition
    }


updateEntity =
    Context.updateComponentsWithOthers
        { func = updateAIVelocity
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentTurn.modifier.get
                |> Component.join ComponentVelocity.modifier.get
                |> Component.join ComponentPosition.modifier.get
                |> Component.join ComponentAI.modifier.get
        , otherComponents =
            Context.select OtherComponents
                |> Context.fromEntities .entities
                |> Context.innerJoin ComponentPlayer.modifier.get
                |> Context.innerJoin ComponentPosition.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentVelocity.modifier.map, .velocity )
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
