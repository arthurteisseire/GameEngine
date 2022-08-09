module SystemCollision exposing (updateWorld)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntityWithOthers2
                collide
                world.entities
                world.positionComponents
                world.positionComponents
                world.velocityComponents
    in
    { world
        | positionComponents = tables.first
        , velocityComponents = tables.second
    }


collide :
    EntityId
    -> Table ComponentPosition
    -> ComponentPosition
    -> ComponentVelocity
    -> Tuple2 ComponentPosition ComponentVelocity
collide _ readTable position velocity =
    let
        movedPos =
            { x = position.x + velocity.x
            , y = position.y + velocity.y
            }

        nextPos =
            if List.member movedPos (valuesTable readTable) then
                position

            else
                movedPos
    in
    toTuple2 nextPos ComponentVelocity.identity
