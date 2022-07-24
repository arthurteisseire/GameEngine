module SystemCollision exposing (update)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


update :
    EntityTable
    -> Table ComponentPosition
    -> Table2 ComponentPosition ComponentVelocity
    -> Table2 ComponentPosition ComponentVelocity
update entityTable readTable writeTables =
    updateEachEntityWithOthers2
        collide
        entityTable
        readTable
        writeTables


collide :
    EntityId
    -> Table ComponentPosition
    -> ComponentPosition
    -> ComponentVelocity
    -> Component2 ComponentPosition ComponentVelocity
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
    toComponent2 nextPos ComponentVelocity.identity
