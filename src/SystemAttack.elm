module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
import Core.Table as Table exposing (Table)
import Vector2
import World exposing (..)


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


updateEntity : EntityId -> World -> World
updateEntity =
    Db.updateComponentsWithOthers
        { func = velocityAttack
        , inputComponents =
            Component.select InputComponents
                |> Component.join .positionComponents
                |> Component.join .velocityComponents
                |> Component.join .animationComponents
        , otherComponents =
            Db.select OtherComponents
                |> Db.fromEntities .entities
                |> Db.innerJoin .positionComponents
        , output =
            Modifier.select
                |> Modifier.join ( attackModifier, .attack )
                |> Modifier.join ( animationModifier, .animation )
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


clear : EntityId -> World -> World
clear entityId world =
    { world | attackComponents = Table.insert entityId Nothing world.attackComponents }
