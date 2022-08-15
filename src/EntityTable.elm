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


foldlEntityTable : (EntityId -> result -> result) -> result -> EntityTable -> result
foldlEntityTable func result (EntityTable _ list) =
    List.foldl
        (\entityId acc -> func entityId acc)
        result
        list


filterEntities : (EntityId -> Bool) -> EntityTable -> EntityTable
filterEntities isGood (EntityTable lastId list) =
    EntityTable lastId (List.filter isGood list)


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


updateEntities :
    { updateComponents : EntityId -> w -> w
    , updateWorld : EntityId -> w -> result -> result
    , world : result
    , entityTable : EntityTable
    , componentTables : EntityTable -> Table w
    }
    -> result
updateEntities { updateComponents, updateWorld, world, entityTable, componentTables } =
    foldlEntities1
        (\entityId w accResult -> updateWorld entityId (updateComponents entityId w) accResult)
        world
        (componentTables entityTable)
        entityTable


updateEntitiesWithOthers :
    { updateComponents : EntityId -> Table r -> w -> w
    , updateWorld : EntityId -> w -> result -> result
    , world : result
    , entityTable : EntityTable
    , readTable : EntityTable -> Table r
    , writeTable : EntityTable -> Table w
    }
    -> result
updateEntitiesWithOthers { updateComponents, updateWorld, world, entityTable, readTable, writeTable } =
    foldlEntities1
        (\entityId w accResult -> updateWorld entityId (updateComponents entityId (readTable entityTable) w) accResult)
        world
        (writeTable entityTable)
        entityTable


from : Table a -> (a -> result) -> EntityTable -> Table result
from table func =
    mapEntities1 (\_ -> func) table


join : Table b -> (EntityTable -> Table (b -> a)) -> EntityTable -> Table a
join table resultTable entityTable =
    foldlEntities2
        (\entityId keyboardInput func accTable -> insertInTable entityId (func keyboardInput) accTable)
        emptyTable
        table
        (resultTable entityTable)
        entityTable


intersectTable3 : (a -> b -> c -> w) -> Table a -> Table b -> Table c -> EntityTable -> Table w
intersectTable3 func =
    foldlEntities3
        (\entityId a b c accTable -> insertInTable entityId (func a b c) accTable)
        emptyTable


intersectTable2 : (a -> b -> w) -> Table a -> Table b -> EntityTable -> Table w
intersectTable2 func =
    foldlEntities2
        (\entityId a b accTable -> insertInTable entityId (func a b) accTable)
        emptyTable



--join : (a -> b -> (c -> w)) -> Table a -> Table b -> EntityTable -> Table (c -> w)
--join : Table a -> Table b -> EntityTable -> Table w
--from : Table a -> EntityTable -> Table w
--from tableA entityTable =


mapEntities3 :
    (EntityId -> a -> b -> c -> result)
    -> Table a
    -> Table b
    -> Table c
    -> EntityTable
    -> Table result
mapEntities3 func =
    foldlEntities3
        (\entityId a b c accTable -> insertInTable entityId (func entityId a b c) accTable)
        emptyTable


mapEntities2 :
    (EntityId -> a -> b -> result)
    -> Table a
    -> Table b
    -> EntityTable
    -> Table result
mapEntities2 func =
    foldlEntities2
        (\entityId a b accTable -> insertInTable entityId (func entityId a b) accTable)
        emptyTable


mapEntities1 : (EntityId -> a -> result) -> Table a -> EntityTable -> Table result
mapEntities1 func =
    foldlEntities1
        (\entityId a accTable -> insertInTable entityId (func entityId a) accTable)
        emptyTable


foldlEntities3 :
    (EntityId -> a -> b -> c -> result -> result)
    -> result
    -> Table a
    -> Table b
    -> Table c
    -> EntityTable
    -> result
foldlEntities3 func result tableA tableB tableC entityTable =
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
    -> Table a
    -> Table b
    -> EntityTable
    -> result
foldlEntities2 func result tableA tableB entityTable =
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


foldlEntities1 : (EntityId -> a -> result -> result) -> result -> Table a -> EntityTable -> result
foldlEntities1 func result table entityTable =
    foldlEntityTable
        (\entityId accResult ->
            Maybe.map
                (\a -> func entityId a accResult)
                (getComponent entityId table)
                |> Maybe.withDefault accResult
        )
        result
        entityTable


setComponent : EntityId -> a -> Table a -> Table a
setComponent =
    insertInTable


getComponent : EntityId -> Table a -> Maybe a
getComponent (EntityId id) (Table dict) =
    Dict.get id dict



-- (Table <-> Dict) Bindings


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
