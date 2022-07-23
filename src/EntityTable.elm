module EntityTable exposing (..)

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


type alias Component2 a b =
    { a : a
    , b : b
    }


type alias Table2New a b =
    Table (Component2 a b)


type alias Table2 a b =
    { tableA : Table a
    , tableB : Table b
    }


getDictTable : Table a -> Dict Int a
getDictTable (Table dict) =
    dict


unionTable : Table a -> Table a -> Table a
unionTable (Table dictHighPriority) (Table dictLowPriority) =
    Table (Dict.union dictHighPriority dictLowPriority)


splitTable2 : Table (Component2 a b) -> Table2 a b
splitTable2 table =
    foldlTable
        (\id comp2 table2 ->
            { tableA = insertInTable (EntityId id) comp2.a table2.tableA
            , tableB = insertInTable (EntityId id) comp2.b table2.tableB
            }
        )
        { tableA = emptyComponentTable, tableB = emptyComponentTable }
        table


intersectTable2 : Table2 a b -> Table (Component2 a b)
intersectTable2 { tableA, tableB } =
    Dict.merge
        (\_ _ t -> t)
        (\id a b t -> Table (Dict.insert id { a = a, b = b } (getDictTable t)))
        (\_ _ t -> t)
        (getDictTable tableA)
        (getDictTable tableB)
        emptyComponentTable


unionTable2 : Table2 a b -> Table2 a b -> Table2 a b
unionTable2 tableHighPriority tableLowPriority =
    { tableA = unionTable tableHighPriority.tableA tableLowPriority.tableA
    , tableB = unionTable tableHighPriority.tableB tableLowPriority.tableB
    }


createFromTable2 : (Table (Component2 a b) -> Table c) -> Table2 a b -> Table c
createFromTable2 func table2 =
    func (intersectTable2 table2)


update2Tables : (Table (Component2 a b) -> Table (Component2 a b)) -> Table2 a b -> Table2 a b
update2Tables func table2 =
    let
        entities : Table (Component2 a b)
        entities =
            mergeTable
                (\_ _ t -> t)
                (\id a b t -> insertInTable id { a = a, b = b } t)
                (\_ _ t -> t)
                table2.tableA
                table2.tableB
                emptyComponentTable

        updatedEntities : Table (Component2 a b)
        updatedEntities =
            func entities

        splitedTable2 : Table2 a b
        splitedTable2 =
            foldlTable
                (\id comp2 fTable2 ->
                    { tableA = insertInTable (EntityId id) comp2.a fTable2.tableA
                    , tableB = insertInTable (EntityId id) comp2.b fTable2.tableB
                    }
                )
                { tableA = emptyComponentTable, tableB = emptyComponentTable }
                updatedEntities

        unionedTable2 : Table2 a b
        unionedTable2 =
            { tableA = unionTable splitedTable2.tableA table2.tableA
            , tableB = unionTable splitedTable2.tableB table2.tableB
            }
    in
    unionedTable2



--unionTable2
--    (splitTable2 (createFromTable2 func table2))
--    table2


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


applyEntityLaw : (EntityId -> Table r1 -> r2 -> w) -> EntityTable -> Table r1 -> Table r2 -> Table w
applyEntityLaw func entities readTable writeTable =
    updateEachEntity
        (\entityId writeEntity -> func entityId (filterEntities entities readTable) writeEntity)
        entities
        writeTable


updateEachEntity : (EntityId -> r -> w) -> EntityTable -> Table r -> Table w
updateEachEntity func entities writeTable =
    mapTable
        (\id writeEntity -> func (EntityId id) writeEntity)
        (filterEntities entities writeTable)


filterEntities : EntityTable -> Table a -> Table a
filterEntities entities table =
    filterTable (\id _ -> doesEntityExist (EntityId id) entities) table


filterTable : (Int -> a -> Bool) -> Table a -> Table a
filterTable isGood (Table dict) =
    Table (Dict.filter isGood dict)


insertInTable : EntityId -> a -> Table a -> Table a
insertInTable (EntityId id) a (Table dict) =
    Table (Dict.insert id a dict)


foldlTable : (Int -> a -> b -> b) -> b -> Table a -> b
foldlTable func acc (Table dict) =
    Dict.foldl func acc dict


valuesTable : Table a -> List a
valuesTable (Table dict) =
    Dict.values dict


mapTable : (Int -> a -> b) -> Table a -> Table b
mapTable func (Table dict) =
    Table (Dict.map func dict)


emptyComponentTable : Table a
emptyComponentTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent entityId component table =
    insertInTable entityId component table


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict
