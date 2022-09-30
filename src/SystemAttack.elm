module SystemAttack exposing (..)

import ComponentAnimation exposing (ComponentAnimation)
import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import Vector2
import World exposing (World)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , attack : ComponentAttack
    , animation : ComponentAnimation
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map4
            (\position velocity attack animation ->
                let
                    components =
                        velocityAttack entityId (Components position velocity attack animation)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                    , attackComponents = insertComponent entityId components.attack world.attackComponents
                    , animationComponents = insertComponent entityId components.animation world.animationComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.velocityComponents)
            (getComponent entityId world.attackComponents)
            (getComponent entityId world.animationComponents)


velocityAttack : EntityId -> Components -> Components
velocityAttack _ components =
    if components.velocity /= ComponentVelocity.identity then
        { position = components.position
        , velocity = components.velocity
        , attack =
            if components.velocity /= ComponentVelocity.identity then
                Just
                    { from = components.position
                    , to = Vector2.add components.position components.velocity
                    }

            else
                Nothing
        , animation = components.animation
        }

    else
        { position = components.position
        , velocity = components.velocity
        , attack = Nothing
        , animation = components.animation
        }
