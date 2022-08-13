module SystemAttack exposing (updateWorld)

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


updateWorld : World -> World
updateWorld world =
    updateEntities
        { updateComponents = velocityAttack
        , updateWorld =
            \entityId { position, velocity, attack } accWorld ->
                { accWorld
                    | positionComponents = setComponent entityId position accWorld.positionComponents
                    , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                    , attackComponents = setComponent entityId attack accWorld.attackComponents
                }
        , world = world
        , entityTable = world.entities
        , componentTables = intersectTable3 Components world.entities world.positionComponents world.velocityComponents world.attackComponents
        }


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
