module EntityTable exposing (..)

--module EntityTable exposing
--    ( EntityId
--    , EntityTable
--    , Table
--    , addEntity
--    , emptyEntityTable
--    , emptyTable
--    , entityIdToString
--    , getComponent
--    , mapEntities1
--    , mapEntities2
--    , mapEntities3
--    , setComponent
--    , unionTable
--    , updateEachEntity2
--    , updateEachEntityWithOthers2
--    , valuesTable
--    )

import CustomTuple exposing (..)
import Dict exposing (Dict)



-- Entity Table


type EntityId
    = EntityId Int


entityIdToString : EntityId -> String
entityIdToString (EntityId id) =
    String.fromInt id


type EntityTable
    = EntityTable Int (List EntityId)


mapFilterEntityTableToTable : (EntityId -> Maybe a) -> EntityTable -> Table a
mapFilterEntityTableToTable func (EntityTable _ list) =
    List.filterMap
        (\(EntityId id) -> Maybe.map (\v -> ( id, v )) (func (EntityId id)))
        list
        |> fromListTable


mapEntityTableToTable : (EntityId -> a) -> EntityTable -> Table a
mapEntityTableToTable func (EntityTable _ list) =
    List.indexedMap
        (\idx a -> ( idx, func a ))
        list
        |> fromListTable


foldlEntityTable : (EntityId -> result -> result) -> result -> EntityTable -> result
foldlEntityTable func result (EntityTable _ list) =
    List.foldl
        (\entityId acc -> func entityId acc)
        result
        list


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


updateEachEntityWithOthers2 :
    (EntityId -> Table r -> a -> b -> Tuple2 a b)
    -> EntityTable
    -> Table r
    -> Table a
    -> Table b
    -> Tuple2 (Table a) (Table b)
updateEachEntityWithOthers2 func entityTable readTable tableA tableB =
    updateEachEntity2
        (\entityId a b -> func entityId (filterEntitiesInTable entityTable readTable) a b)
        entityTable
        tableA
        tableB


updateEachEntity2 :
    (EntityId -> a -> b -> Tuple2 a b)
    -> EntityTable
    -> Table a
    -> Table b
    -> Tuple2 (Table a) (Table b)
updateEachEntity2 func entityTable tableA tableB =
    let
        table2 : Table (Tuple2 a b)
        table2 =
            mapEntities2
                func
                entityTable
                tableA
                tableB
    in
    toTuple2
        (unionTable (mapEntities1 (\_ -> .first) entityTable table2) tableA)
        (unionTable (mapEntities1 (\_ -> .second) entityTable table2) tableB)


intersectTable3 :
    EntityTable
    -> Table a
    -> Table b
    -> Table c
    -> Table (Tuple3 a b c)
intersectTable3 =
    foldlEntities3
        (\entityId a b c accTable -> insertInTable entityId (toTuple3 a b c) accTable)
        emptyTable


intersectTable2 :
    EntityTable
    -> Table a
    -> Table b
    -> Table (Tuple2 a b)
intersectTable2 =
    foldlEntities2
        (\entityId a b accTable -> insertInTable entityId (toTuple2 a b) accTable)
        emptyTable


mapEntities3 :
    (EntityId -> a -> b -> c -> result)
    -> EntityTable
    -> Table a
    -> Table b
    -> Table c
    -> Table result
mapEntities3 func =
    foldlEntities3
        (\entityId a b c accTable -> insertInTable entityId (func entityId a b c) accTable)
        emptyTable


mapEntities2 :
    (EntityId -> a -> b -> result)
    -> EntityTable
    -> Table a
    -> Table b
    -> Table result
mapEntities2 func =
    foldlEntities2
        (\entityId a b accTable -> insertInTable entityId (func entityId a b) accTable)
        emptyTable


mapEntities1 : (EntityId -> a -> result) -> EntityTable -> Table a -> Table result
mapEntities1 func =
    foldlEntities1
        (\entityId a accTable -> insertInTable entityId (func entityId a) accTable)
        emptyTable


foldlEntities3 :
    (EntityId -> a -> b -> c -> result -> result)
    -> result
    -> EntityTable
    -> Table a
    -> Table b
    -> Table c
    -> result
foldlEntities3 func result entityTable tableA tableB tableC =
    foldlEntityTable
        (\entityId accResult ->
            Maybe.map3
                (\a b c -> func entityId a b c accResult)
                (getComponent entityId tableA)
                (getComponent entityId tableB)
                (getComponent entityId tableC)
                |> Maybe.withDefault accResult
        )
        result
        entityTable


foldlEntities2 :
    (EntityId -> a -> b -> result -> result)
    -> result
    -> EntityTable
    -> Table a
    -> Table b
    -> result
foldlEntities2 func result entityTable tableA tableB =
    foldlEntityTable
        (\entityId accResult ->
            Maybe.map2
                (\a b -> func entityId a b accResult)
                (getComponent entityId tableA)
                (getComponent entityId tableB)
                |> Maybe.withDefault accResult
        )
        result
        entityTable


foldlEntities1 : (EntityId -> a -> result -> result) -> result -> EntityTable -> Table a -> result
foldlEntities1 func result entityTable table =
    foldlEntityTable
        (\entityId accResult ->
            Maybe.map
                (\a -> func entityId a accResult)
                (getComponent entityId table)
                |> Maybe.withDefault accResult
        )
        result
        entityTable


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
setComponent =
    insertInTable


getComponent : EntityId -> Table a -> Maybe a
getComponent (EntityId id) (Table dict) =
    Dict.get id dict


updateTable : EntityId -> (Maybe v -> Maybe v) -> Table v -> Table v
updateTable (EntityId id) func (Table dict) =
    Dict.update id func dict
        |> Table


mapTable : (EntityId -> a -> b) -> Table a -> Table b
mapTable func (Table dict) =
    Dict.map
        (\id -> func (EntityId id))
        dict
        |> Table


fromDictTable : Dict Int a -> Table a
fromDictTable dict =
    Dict.foldl
        (\id a table -> insertInTable (EntityId id) a table)
        emptyTable
        dict


fromListTable : List ( Int, a ) -> Table a
fromListTable list =
    list
        |> Dict.fromList
        |> fromDictTable
