module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Vector2


type alias OutputComponents =
    { attack : ComponentAttack
    , animation : ComponentAnimation
    }


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , animation : ComponentAnimation
    }


type alias OtherComponents =
    { position : ComponentPosition
    }


updateEntity =
    Context.updateComponentsWithOthers
        { func = velocityAttack
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentPosition.modifier.get
                |> Component.join ComponentVelocity.modifier.get
                |> Component.join ComponentAnimation.modifier.get
        , otherComponents =
            Context.select OtherComponents
                |> Context.fromEntities .entities
                |> Context.innerJoin ComponentPosition.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentAttack.modifier.map, .attack )
                |> ComponentTable.joinModifier ( ComponentAnimation.modifier.map, .animation )
        }


velocityAttack : Table OtherComponents -> InputComponents -> OutputComponents
velocityAttack others { position, velocity, animation } =
    let
        attackPos =
            Vector2.add position velocity
    in
    if velocity /= ComponentVelocity.identity && Table.hasValueIf (\pos other -> pos == other.position) attackPos others then
        { attack =
            Just
                { from = position
                , to = Vector2.add position velocity
                }
        , animation =
            ComponentAnimation.attackAnimation velocity
        }

    else
        { attack = Nothing
        , animation = animation
        }


clear entityId context =
    ComponentAttack.modifier.map (ComponentTable.insert entityId Nothing) context
