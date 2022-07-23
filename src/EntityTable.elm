module EntityTable exposing
    ( Component2
    , EntityId
    , EntityTable
    , Table
    , Table2
    , addEntity
    , emptyEntityTable
    , emptyTable
    , entityIdToString
    , getComponent
    , mapEntities1
    , mapEntities2
    , setComponent
    , updateEachEntity2
    , updateEachEntityWithOthers2
    , valuesTable
    )

import Dict exposing (Dict)



-- Entity Table


type EntityId
    = EntityId Int


entityIdToString : EntityId -> String
entityIdToString (EntityId id) =
    String.fromInt id


type EntityTable
    = EntityTable Int (List EntityId)


emptyEntityTable : EntityTable
emptyEntityTable =
    EntityTable 0 []


doesEntityExist : EntityId -> EntityTable -> Bool
doesEntityExist entityId (EntityTable _ list) =
    List.member entityId list


addEntity : EntityTable -> ( EntityTable, EntityId )
addEntity (EntityTable nextId entities) =
    ( EntityTable (nextId + 1) (EntityId nextId :: entities)
    , EntityId nextId
    )



-- Component Table


type Table a
    = Table (Dict Int a)


type alias Table2 a b =
    { tableA : Table a
    , tableB : Table b
    }


type alias Component2 a b =
    { a : a
    , b : b
    }


updateEachEntityWithOthers2 : (EntityId -> Table a -> Component2 a b -> Component2 a b) -> EntityTable -> Table a -> Table2 a b -> Table2 a b
updateEachEntityWithOthers2 func entityTable readTable writeTable =
    updateEachEntity2
        (\entityId comp2 -> func entityId (filterEntitiesInTable entityTable readTable) comp2)
        entityTable
        writeTable


updateEachEntity2 : (EntityId -> Component2 a b -> Component2 a b) -> EntityTable -> Table2 a b -> Table2 a b
updateEachEntity2 func entityTable writeTables =
    let
        updatedEntities =
            mapEntities2
                func
                entityTable
                writeTables

        splitedTable2 =
            foldlTable
                (\entityId comp2 table2 ->
                    { tableA = insertInTable entityId comp2.a table2.tableA
                    , tableB = insertInTable entityId comp2.b table2.tableB
                    }
                )
                { tableA = emptyTable, tableB = emptyTable }
                updatedEntities

        unionedTable2 =
            { tableA = unionTable splitedTable2.tableA writeTables.tableA
            , tableB = unionTable splitedTable2.tableB writeTables.tableB
            }
    in
    unionedTable2


mapEntities2 : (EntityId -> Component2 a b -> result) -> EntityTable -> Table2 a b -> Table result
mapEntities2 func entityTable table2 =
    mergeTable
        (\_ _ t2 -> t2)
        (\entityId a b t2 -> insertInTable entityId (func entityId { a = a, b = b }) t2)
        (\_ _ t2 -> t2)
        (filterEntitiesInTable entityTable table2.tableA)
        (filterEntitiesInTable entityTable table2.tableB)
        emptyTable


mapEntities1 : (EntityId -> a -> result) -> EntityTable -> Table a -> Table result
mapEntities1 func entityTable table =
    foldlTable
        (\entityId a accTable -> insertInTable entityId (func entityId a) accTable)
        emptyTable
        (filterEntitiesInTable entityTable table)


mergeTable :
    (EntityId -> a -> result -> result)
    -> (EntityId -> a -> b -> result -> result)
    -> (EntityId -> b -> result -> result)
    -> Table a
    -> Table b
    -> result
    -> result
mergeTable leftStep bothStep rightStep (Table leftDict) (Table rightDict) initialResult =
    Dict.merge
        (\id a t -> leftStep (EntityId id) a t)
        (\id a b t -> bothStep (EntityId id) a b t)
        (\id b t -> rightStep (EntityId id) b t)
        leftDict
        rightDict
        initialResult


unionTable : Table a -> Table a -> Table a
unionTable (Table dictHighPriority) (Table dictLowPriority) =
    Table (Dict.union dictHighPriority dictLowPriority)


filterEntitiesInTable : EntityTable -> Table a -> Table a
filterEntitiesInTable entities table =
    filterTable (\entityId _ -> doesEntityExist entityId entities) table


filterTable : (EntityId -> a -> Bool) -> Table a -> Table a
filterTable isGood (Table dict) =
    Table (Dict.filter (\id a -> isGood (EntityId id) a) dict)


insertInTable : EntityId -> a -> Table a -> Table a
insertInTable (EntityId id) a (Table dict) =
    Table (Dict.insert id a dict)


foldlTable : (EntityId -> a -> b -> b) -> b -> Table a -> b
foldlTable func acc (Table dict) =
    Dict.foldl (\id a b -> func (EntityId id) a b) acc dict


valuesTable : Table a -> List a
valuesTable (Table dict) =
    Dict.values dict


emptyTable : Table a
emptyTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent entityId component table =
    insertInTable entityId component table


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict
