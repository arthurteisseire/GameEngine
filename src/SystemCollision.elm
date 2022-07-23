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
        collideEntity
        entityTable
        readTable
        writeTables


collideEntity :
    EntityId
    -> Table ComponentPosition
    -> Component2 ComponentPosition ComponentVelocity
    -> Component2 ComponentPosition ComponentVelocity
collideEntity _ readTable component2 =
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



--mergeTable
--    (\entityId pos table2 ->
--        { tableA = insertInTable entityId pos table2.tableA
--        , tableB = table2.tableB
--        }
--    )
--    (\entityId pos vel table2 ->
--        let
--            comp2 =
--                if doesEntityExist entityId entityTable then
--                    func { a = pos, b = vel }
--
--                else
--                    { a = pos, b = vel }
--        in
--        { tableA = insertInTable entityId comp2.a table2.tableA
--        , tableB = insertInTable entityId comp2.b table2.tableB
--        }
--    )
--    (\entityId vel table2 ->
--        { tableA = table2.tableA
--        , tableB = insertInTable entityId vel table2.tableB
--        }
--    )
--    writeTables.tableA
--    writeTables.tableB
--    { tableA = emptyComponentTable
--    , tableB = emptyComponentTable
--    }
