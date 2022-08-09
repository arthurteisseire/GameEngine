module SystemCollision exposing (update)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)


update :
    EntityTable
    -> Table ComponentPosition
    -> Table ComponentPosition
    -> Table ComponentVelocity
    -> Tuple2 (Table ComponentPosition) (Table ComponentVelocity)
update entityTable readTable positionTable velocityTable =
    updateEachEntityWithOthers2
        collide
        entityTable
        readTable
        positionTable
        velocityTable


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
