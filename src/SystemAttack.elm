module SystemAttack exposing (..)

import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import World exposing (World)


type alias Components =
    { position : ComponentPosition
    , velocity : ComponentVelocity
    , attack : ComponentAttack
    }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map3
            (\position velocity attack ->
                let
                    components =
                        velocityAttack entityId (Components position velocity attack)
                in
                { world
                    | positionComponents = insertComponent entityId components.position world.positionComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                    , attackComponents = insertComponent entityId components.attack world.attackComponents
                }
            )
            (getComponent entityId world.positionComponents)
            (getComponent entityId world.velocityComponents)
            (getComponent entityId world.attackComponents)


velocityAttack : EntityId -> Components -> Components
velocityAttack _ { position, velocity, attack } =
    { position = position
    , velocity = velocity
    , attack =
        if velocity /= ComponentVelocity.identity then
            Just { x = position.x + velocity.x, y = position.y + velocity.y }

        else
            Nothing
    }
