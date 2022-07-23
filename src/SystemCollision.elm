module SystemCollision exposing (update)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


update :
    EntityTable
    -> Table ComponentPosition
    -> Table2 ComponentPosition ComponentVelocity
    -> Table2 ComponentPosition ComponentVelocity
update entities readTable writeTables =
    update2Tables
        (collide entities readTable)
        writeTables


collide :
    EntityTable
    -> Table ComponentPosition
    -> Table (Component2 ComponentPosition ComponentVelocity)
    -> Table (Component2 ComponentPosition ComponentVelocity)
collide entities readTable writeTable =
    applyEntityLaw
        (\_ read comp2 -> collideEntity read comp2)
        entities
        readTable
        writeTable


collideEntity :
    Table ComponentPosition
    -> Component2 ComponentPosition ComponentVelocity
    -> Component2 ComponentPosition ComponentVelocity
collideEntity readTable component2 =
    let
        position =
            component2.a

        velocity =
            component2.b

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
    { a = nextPos
    , b = ComponentVelocity.identity
    }
