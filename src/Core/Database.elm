module Core.Database exposing (..)

import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet(..))
import Core.Table as Table exposing (Table)



-- Update entities


mapComponentsFull :
    { func : inputs -> result
    , inputComponents : EntityId -> db -> inputs
    }
    -> EntityId
    -> db
    -> result
mapComponentsFull { func, inputComponents } entityId db =
    func (inputComponents entityId db)


mapComponents :
    { func : inputs -> result
    , inputComponents : EntityId -> db -> Maybe inputs
    }
    -> EntityId
    -> db
    -> Maybe result
mapComponents { func, inputComponents } entityId db =
    Maybe.map
        func
        (inputComponents entityId db)


updateComponents :
    { func : inputs -> outputs
    , inputComponents : EntityId -> db -> Maybe inputs
    , output : outputs -> EntityId -> db -> db
    }
    -> EntityId
    -> db
    -> db
updateComponents { func, inputComponents, output } entityId db =
    mapComponents
        { func = \components -> output (func components) entityId db
        , inputComponents = inputComponents
        }
        entityId
        db
        |> Maybe.withDefault db


updateComponentsWithOthers :
    { func : Table others -> inputs -> outputs
    , inputComponents : EntityId -> db -> Maybe inputs
    , otherComponents : db -> Table others
    , output : outputs -> EntityId -> db -> db
    }
    -> EntityId
    -> db
    -> db
updateComponentsWithOthers { func, inputComponents, otherComponents, output } entityId db =
    updateComponents
        { func = func (otherComponents db |> Table.remove entityId)
        , inputComponents = inputComponents
        , output = output
        }
        entityId
        db



-- Table relations


select : a -> a
select =
    Table.select


fromEntities : (db -> EntitySet) -> a -> db -> Table a
fromEntities getEntitySet a db =
    EntitySet.foldl
        (\entity table -> Table.insert entity a table)
        Table.empty
        (getEntitySet db)


from : (db -> Table a) -> (a -> result) -> db -> Table result
from getTable func db =
    Table.from (getTable db) func


innerJoin : (db -> Table a) -> (db -> Table (a -> result)) -> db -> Table result
innerJoin getTable getNextTable db =
    Table.innerJoin (getTable db) (getNextTable db)



-- Entities


mapEntitiesInTable : (EntityId -> a -> result) -> Table a -> EntitySet -> Table result
mapEntitiesInTable func table entitySet =
    Table.map func (filterEntitiesInTable entitySet table)


filterEntitiesInTable : EntitySet -> Table a -> Table a
filterEntitiesInTable (EntitySet _ entityList) table =
    Table.filter (\entityId _ -> List.member entityId entityList) table


