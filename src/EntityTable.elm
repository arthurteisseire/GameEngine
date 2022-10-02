module EntityTable exposing (..)

import Dict exposing (Dict)



-- Entity Table


type EntityId
    = EntityId Int


entityIdToString : EntityId -> String
entityIdToString (EntityId id) =
    String.fromInt id


type EntitySet
    = EntitySet Int (List EntityId)


foldlEntitySet : (EntityId -> result -> result) -> result -> EntitySet -> result
foldlEntitySet func result (EntitySet _ list) =
    List.foldl
        (\entityId acc -> func entityId acc)
        result
        list


remove : EntityId -> EntitySet -> EntitySet
remove entityId (EntitySet lastId list) =
    EntitySet lastId (List.filter ((/=) entityId) list)


removeIf : Bool -> EntityId -> EntitySet -> EntitySet
removeIf isBad entityId entitySet =
    if isBad then
        remove entityId entitySet

    else
        entitySet


filterEntities : (EntityId -> Bool) -> EntitySet -> EntitySet
filterEntities isGood (EntitySet lastId list) =
    EntitySet lastId (List.filter isGood list)


emptyEntitySet : EntitySet
emptyEntitySet =
    EntitySet 0 []


doesEntityExist : EntityId -> EntitySet -> Bool
doesEntityExist entityId (EntitySet _ list) =
    List.member entityId list


addEntity : EntitySet -> ( EntitySet, EntityId )
addEntity (EntitySet nextId entities) =
    ( EntitySet (nextId + 1) (EntityId nextId :: entities)
    , EntityId nextId
    )



-- Component Table


type Table a
    = Table (Dict Int a)


updateEntities :
    { updateComponents : EntityId -> w -> w
    , updateWorld : EntityId -> w -> result -> result
    , world : result
    , entityTable : EntitySet
    , componentTables : EntitySet -> Table w
    }
    -> result
updateEntities { updateComponents, updateWorld, world, entityTable, componentTables } =
    foldlEntities
        (\entityId a accWorld -> updateWorld entityId (updateComponents entityId a) accWorld)
        world
        (componentTables entityTable)
        entityTable


updateEntitiesWithOthers :
    { updateComponents : EntityId -> Table r -> w -> w
    , updateWorld : EntityId -> w -> result -> result
    , world : result
    , entityTable : EntitySet
    , readTable : EntitySet -> Table r
    , writeTable : EntitySet -> Table w
    }
    -> result
updateEntitiesWithOthers { updateComponents, updateWorld, world, entityTable, readTable, writeTable } =
    foldlEntities
        (\entityId w accResult -> updateWorld entityId (updateComponents entityId (readTable entityTable) w) accResult)
        world
        (writeTable entityTable)
        entityTable


from : Table a -> (a -> result) -> EntitySet -> Table result
from table func =
    mapEntities (\_ -> func) table


join : Table a -> (EntitySet -> Table (a -> result)) -> EntitySet -> Table result
join table resultTable entityTable =
    foldlEntities2
        (\entityId a func accTable -> insertInTable entityId (func a) accTable)
        emptyTable
        table
        (resultTable entityTable)
        entityTable


fullJoin : Table a -> (EntitySet -> Table (Maybe a -> result)) -> EntitySet -> Table result
fullJoin table resultTable entityTable =
    foldlEntities2Full
        (\entityId a func accTable -> insertInTable entityId (func a) accTable)
        emptyTable
        table
        (resultTable entityTable)
        entityTable


mapEntities : (EntityId -> a -> result) -> Table a -> EntitySet -> Table result
mapEntities func =
    foldlEntities
        (\entityId a accTable -> insertInTable entityId (func entityId a) accTable)
        emptyTable


foldlEntities : (EntityId -> a -> result -> result) -> result -> Table a -> EntitySet -> result
foldlEntities func result table entityTable =
    foldlEntitySet
        (\entityId accResult ->
            case getComponent entityId table of
                Just component ->
                    func entityId component accResult

                Nothing ->
                    accResult
        )
        result
        entityTable


foldlEntities2 :
    (EntityId -> a -> b -> result -> result)
    -> result
    -> Table a
    -> Table b
    -> EntitySet
    -> result
foldlEntities2 func result tableA tableB entityTable =
    foldlEntitySet
        (\entityId accResult ->
            Maybe.map2
                (\a b -> func entityId a b accResult)
                (getComponent entityId tableA)
                (getComponent entityId tableB)
                |> Maybe.withDefault accResult
        )
        result
        entityTable


foldlEntities2Full :
    (EntityId -> Maybe a -> b -> result -> result)
    -> result
    -> Table a
    -> Table b
    -> EntitySet
    -> result
foldlEntities2Full func result tableA tableB entityTable =
    foldlEntitySet
        (\entityId accResult ->
            (case getComponent entityId tableB of
                Just b ->
                    Just <| func entityId (getComponent entityId tableA) b accResult

                Nothing ->
                    Nothing
            )
                |> Maybe.withDefault accResult
        )
        result
        entityTable


updateComponent : EntityId -> a -> Table a -> Table a
updateComponent entityId component table =
    case getComponent entityId table of
        Just _ ->
            insertComponent entityId component table

        Nothing ->
            table


insertComponent : EntityId -> a -> Table a -> Table a
insertComponent =
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


filterEntitiesInTable : EntitySet -> Table a -> Table a
filterEntitiesInTable entities table =
    filterTable (\entityId _ -> doesEntityExist entityId entities) table


filterTable : (EntityId -> a -> Bool) -> Table a -> Table a
filterTable isGood (Table dict) =
    Table (Dict.filter (\id a -> isGood (EntityId id) a) dict)


insertInTable : EntityId -> a -> Table a -> Table a
insertInTable (EntityId id) a (Table dict) =
    Table (Dict.insert id a dict)


removeInTable : EntityId -> Table a -> Table a
removeInTable (EntityId id) (Table dict) =
    Table (Dict.remove id dict)


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


hasValueInTable : a -> Table a -> Bool
hasValueInTable a table =
    filterTable
        (\_ value -> a == value)
        table
        /= emptyTable


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
