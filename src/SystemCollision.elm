module SystemCollision exposing (update)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


update : EntityTable -> Table2 ComponentPosition ComponentVelocity -> Table2 ComponentPosition ComponentVelocity
update entities tables =
    update2Tables
        entities
        (collide (filterEntities entities tables.a))
        tables


collide : Table ComponentPosition -> Table (Component2 ComponentPosition ComponentVelocity) -> Table (Component2 ComponentPosition ComponentVelocity)
collide allEntities entitiesWithVelocity =
    newMapTable
        (\_ entity -> collideEntity allEntities entity)
        entitiesWithVelocity


collideEntity : Table ComponentPosition -> Component2 ComponentPosition ComponentVelocity -> Component2 ComponentPosition ComponentVelocity
collideEntity allEntities component2 =
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
            if List.member movedPos (valuesTable allEntities) then
                position

            else
                movedPos
    in
    { a = nextPos
    , b = ComponentVelocity.identity
    }
