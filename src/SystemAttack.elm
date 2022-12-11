module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (..)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , attack : ComponentAttack
    , animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity =
    update4Components velocityAttack
        Components
        positionComponent
        velocityComponent
        attackComponent
        animationComponent


velocityAttack : EntityId -> Components -> Components
velocityAttack _ { position, velocity, attack, animation } =
    { position = position
    , velocity = velocity
    , attack =
        if velocity /= ComponentVelocity.identity then
            Just
                { from = position.currentPos
                , to = Vector2.add position.currentPos velocity
                }

        else
            Nothing
    , animation = animation
    }


clear : EntityId -> World -> World
clear entityId world =
    { world | attackComponents = updateComponent entityId Nothing world.attackComponents }
