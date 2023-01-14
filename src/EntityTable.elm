module EntityTable exposing (..)

import Dict exposing (Dict)



-- Database


type alias SimpleModifier a b =
    { get : b -> a
    , set : a -> b -> b
    }


type alias Modifier a b =
    { get : b -> a
    , set : a -> b -> b
    , map : (a -> a) -> b -> b
    }


tableModifier : SimpleModifier a b -> Modifier a b
tableModifier =
    embellishedModifier


embellishedModifier : SimpleModifier a b -> Modifier a b
embellishedModifier modifier =
    { get = modifier.get
    , set = modifier.set
    , map = \func b -> modifier.set (func (modifier.get b)) b
    }


updateComponents :
    { func : inputs -> outputs
    , inputComponents : EntityId -> db -> Maybe inputs
    , output : outputs -> EntityId -> db -> db
    }
    -> EntityId
    -> db
    -> db
updateComponents { func, inputComponents, output } entityId db =
    Maybe.map
        (\components -> output (func components) entityId db)
        (inputComponents entityId db)
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
        { func = func (otherComponents db |> remove entityId)
        , inputComponents = inputComponents
        , output = output
        }
        entityId
        db



-- Get Components


toInputComponents : a -> EntityId -> db -> Maybe a
toInputComponents a _ _ =
    Just a


withInput : (db -> Table a) -> (EntityId -> db -> Maybe (a -> b)) -> EntityId -> db -> Maybe b
withInput getTable nestedFunc entityId db =
    nestedFunc entityId db
        |> Maybe.andThen
            (\func -> mapComponent func entityId (getTable db))


toOutputComponents : b -> EntityId -> db -> db
toOutputComponents _ _ db =
    db


withOutput :
    ( Modifier (Table a) db, b -> a )
    -> (b -> EntityId -> db -> db)
    -> b
    -> EntityId
    -> db
    -> db
withOutput modifier previousUpdater outputComponents entityId world =
    world
        |> previousUpdater outputComponents entityId
        |> updateComponentInTable modifier outputComponents entityId


updateComponentInTable : ( Modifier (Table a) db, b -> a ) -> b -> EntityId -> db -> db
updateComponentInTable ( tableA, getA ) outputComponents entityId =
    tableA.map (updateComponent entityId (getA outputComponents))



-- Entity Table


type EntityId
    = EntityId Int


entityIdToString : EntityId -> String
entityIdToString (EntityId id) =
    String.fromInt id


type EntitySet
    = EntitySet Int (List EntityId)


mapEntitySet : (EntityId -> result) -> EntitySet -> List result
mapEntitySet func entitySet =
    foldlEntitySet
        (\entityId acc -> func entityId :: acc)
        []
        entitySet


foldlEntitySet : (EntityId -> result -> result) -> result -> EntitySet -> result
foldlEntitySet func result (EntitySet _ list) =
    List.foldl
        (\entityId acc -> func entityId acc)
        result
        list


removeEntity : EntityId -> EntitySet -> EntitySet
removeEntity entityId (EntitySet lastId list) =
    EntitySet lastId (List.filter ((/=) entityId) list)


removeEntityIf : Bool -> EntityId -> EntitySet -> EntitySet
removeEntityIf isBad entityId entitySet =
    if isBad then
        removeEntity entityId entitySet

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


addNEntities : Int -> EntitySet -> ( EntitySet, List EntityId )
addNEntities n entitySet =
    let
        func : Int -> EntitySet -> List EntityId -> ( EntitySet, List EntityId )
        func currentN currentEntitySet currentEntityIds =
            case currentN of
                0 ->
                    ( currentEntitySet, currentEntityIds )

                _ ->
                    let
                        ( newSet, entityId ) =
                            addEntity currentEntitySet
                    in
                    func (currentN - 1) newSet (entityId :: currentEntityIds)
    in
    func n entitySet []



-- Component Table


type Table a
    = Table (Dict Int a)


mapEntities : (EntityId -> a -> result) -> Table a -> EntitySet -> Table result
mapEntities func table entityTable =
    mapTable func (filterEntitiesInTable entityTable table)


select : a -> a
select =
    identity


using : (db -> EntitySet) -> a -> db -> Table a
using toEntities func world =
    foldlEntitySet
        (\entityId table -> updateComponent entityId func table)
        emptyTable
        (toEntities world)


andFrom : (db -> Table a) -> (db -> Table (a -> result)) -> db -> Table result
andFrom getTable getNextTable world =
    andMapTable (getTable world) (getNextTable world)


andMapTable : Table a -> Table (a -> result) -> Table result
andMapTable =
    mapTable2 (|>)


fullJoin : Table a -> Table (Maybe a -> result) -> Table result
fullJoin =
    mapTable2Full (|>)


mapTable2 :
    (a -> b -> result)
    -> Table a
    -> Table b
    -> Table result
mapTable2 func tableA tableB =
    foldlTable2
        (\entityId a b result -> updateComponent entityId (func a b) result)
        emptyTable
        tableA
        tableB


foldlTable2 :
    (EntityId -> a -> b -> result -> result)
    -> result
    -> Table a
    -> Table b
    -> result
foldlTable2 func result tableA tableB =
    foldlTable
        (\entityId a accResult ->
            case getComponent entityId tableB of
                Just b ->
                    func entityId a b accResult

                Nothing ->
                    accResult
        )
        result
        tableA


mapTable2Full :
    (Maybe a -> b -> result)
    -> Table a
    -> Table b
    -> Table result
mapTable2Full func tableA tableB =
    foldlTable2Full
        (\entityId a b result -> updateComponent entityId (func a b) result)
        emptyTable
        tableA
        tableB


foldlTable2Full :
    (EntityId -> Maybe a -> b -> result -> result)
    -> result
    -> Table a
    -> Table b
    -> result
foldlTable2Full func result tableA tableB =
    foldlTable
        (\entityId a accResult ->
            (case getComponent entityId tableB of
                Just b ->
                    Just <| func entityId (Just a) b accResult

                Nothing ->
                    Nothing
            )
                |> Maybe.withDefault accResult
        )
        result
        tableA


insertComponent : EntityId -> a -> Table a -> Table a
insertComponent =
    updateComponent


getComponent : EntityId -> Table a -> Maybe a
getComponent (EntityId id) (Table dict) =
    Dict.get id dict


mapComponent : (a -> b) -> EntityId -> Table a -> Maybe b
mapComponent func entityId table =
    Maybe.map func (getComponent entityId table)



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


updateComponent : EntityId -> a -> Table a -> Table a
updateComponent (EntityId id) a (Table dict) =
    Table (Dict.insert id a dict)


remove : EntityId -> Table a -> Table a
remove (EntityId id) (Table dict) =
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
hasValueInTable =
    hasValueInTableIf (==)


hasValueInTableIf : (a -> a -> Bool) -> a -> Table a -> Bool
hasValueInTableIf predicate a table =
    filterTable
        (\_ value -> predicate a value)
        table
        /= emptyTable


fromDictTable : Dict Int a -> Table a
fromDictTable dict =
    Dict.foldl
        (\id a table -> updateComponent (EntityId id) a table)
        emptyTable
        dict


fromListTable : List ( Int, a ) -> Table a
fromListTable list =
    list
        |> Dict.fromList
        |> fromDictTable
