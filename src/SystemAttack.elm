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
        velocityAttack
        (\entityId { position, velocity, attack } accWorld ->
            { accWorld
                | positionComponents = setComponent entityId position accWorld.positionComponents
                , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                , attackComponents = setComponent entityId attack accWorld.attackComponents
            }
        )
        world.entities
        (intersectTable3 Components world.entities world.positionComponents world.velocityComponents world.attackComponents)
        world


velocityAttack : EntityId -> Components -> Components
velocityAttack _ { position, velocity, attack } =
    if velocity /= ComponentVelocity.identity then
        { position = position
        , velocity = velocity
        , attack = Just { x = position.x + velocity.x, y = position.y + velocity.y }
        }

    else
        { position = position
        , velocity = velocity
        , attack = Nothing
        }



--velocityAttack :
--    EntityId
--    -> ComponentPosition
--    -> ComponentVelocity
--    -> ComponentAttack
--    -> Tuple3 ComponentPosition ComponentVelocity ComponentAttack
--velocityAttack _ position velocity attack =
--    if velocity /= ComponentVelocity.identity then
--        toTuple3 position velocity (Just { x = position.x + velocity.x, y = position.y + velocity.y })
--
--    else
--        toTuple3 position velocity Nothing
