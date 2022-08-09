module SystemAttack exposing (updateWorld)

import ComponentAttack exposing (ComponentAttack)
import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntity3
                velocityAttack
                world.entities
                world.positionComponents
                world.velocityComponents
                world.attackComponents
    in
    { world
        | positionComponents = tables.first
        , velocityComponents = tables.second
        , attackComponents = tables.third
    }


velocityAttack :
    EntityId
    -> ComponentPosition
    -> ComponentVelocity
    -> ComponentAttack
    -> Tuple3 ComponentPosition ComponentVelocity ComponentAttack
velocityAttack _ position velocity attack =
    if velocity /= ComponentVelocity.identity then
        toTuple3 position velocity (Just { x = position.x + velocity.x, y = position.y + velocity.y })

    else
        toTuple3 position velocity Nothing
