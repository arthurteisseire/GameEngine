module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias OutputComponents =
    { attack : ComponentAttack
    }


type alias InputComponents =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    updateComponentsNew
        { db = world
        , entityId = entityId
        , func = velocityAttack entityId
        , inputComponents =
            Just InputComponents
                |> withComponent entityId world.positionComponents
                |> withComponent entityId world.velocityComponents
                |> withComponent entityId world.animationComponents
        , output =
            update1ComponentNew
                attackComponent
        }


velocityAttack : EntityId -> InputComponents -> OutputComponents
velocityAttack _ { position, velocity, animation } =
    { attack =
        if velocity /= ComponentVelocity.identity then
            Just
                { from = position.currentPos
                , to = Vector2.add position.currentPos velocity
                }

        else
            Nothing
    }


clear : EntityId -> World -> World
clear entityId world =
    { world | attackComponents = updateComponent entityId Nothing world.attackComponents }
