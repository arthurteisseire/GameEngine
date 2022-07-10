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


addEntity : EntityTable -> ( EntityTable, EntityId )
addEntity (EntityTable nextId entities) =
    ( EntityTable (nextId + 1) (EntityId nextId :: entities)
    , EntityId nextId
    )


foldlEntityTable : (EntityId -> a -> a) -> a -> EntityTable -> a
foldlEntityTable f a (EntityTable _ entities) =
    List.foldl f a entities


mapEntityTable : (EntityId -> a) -> EntityTable -> List a
mapEntityTable f (EntityTable _ entities) =
    List.map f entities


filterMapEntityTable : (EntityId -> Maybe a) -> EntityTable -> List a
filterMapEntityTable f (EntityTable _ entities) =
    List.filterMap f entities



-- Component Table


type Table a
    = Table (Dict Int a)



--type Component2 a b
--    = Component2
--        { a : a
--        , b : b
--
--        --, map : (Component2 a b -> c) -> EntityId -> Component2 a b
--        }
--
--
--mapComponent2 : (a -> b -> b) -> Component2 a b -> Component2 a b
--mapComponent2 f (Component2 component2) =
--    Component2
--        { a = component2.a
--        , b = f component2.a component2.b
--        }
--
--
--mapTableNew : (Component2 a b -> EntityId -> Component2 a b) -> EntityId -> Table (Component2 a b) -> Table (Component2 a b)
--mapTableNew f (EntityId id) (Table dict) =
--    case Dict.get id dict of
--        Just component2 ->
--            Table (Dict.insert id (f component2) dict)


type alias Component2 a b =
    { a : a
    , b : b
    }


type alias Table2New a b =
    Table (Component2 a b)


type alias Table2 a b =
    { a : Table a
    , b : Table b
    }


leftStep : Int -> a -> Table (Component2 a b) -> Table (Component2 a b)
leftStep _ _ table =
    table


bothStep : Int -> a -> b -> Table (Component2 a b) -> Table (Component2 a b)
bothStep id a b (Table dict) =
    Table (Dict.insert id { a = a, b = b } dict)


rightStep : Int -> b -> Table (Component2 a b) -> Table (Component2 a b)
rightStep _ _ table =
    table


getDictTable : Table a -> Dict Int a
getDictTable (Table dict) =
    dict


mergeTable : Table2 a b -> Table (Component2 a b)
mergeTable { a, b } =
    Dict.merge (\_ _ t -> t) bothStep (\_ _ t -> t) (getDictTable a) (getDictTable b) emptyComponentTable


splitTable : Table (Component2 a b) -> Table2 a b
splitTable table =
    foldlTable
        (\id comp2 table2 -> { a = insertInTable id comp2.a table2.a, b = insertInTable id comp2.b table2.b })
        { a = emptyComponentTable, b = emptyComponentTable }
        table


update2Tables : (Table (Component2 a b) -> Table (Component2 a b)) -> Table2 a b -> Table2 a b
update2Tables func table2 =
    splitTable (func (mergeTable table2))


insertInTable : Int -> a -> Table a -> Table a
insertInTable id a (Table dict) =
    Table (Dict.insert id a dict)


foldlTable : (Int -> a -> b -> b) -> b -> Table a -> b
foldlTable func acc (Table dict) =
    Dict.foldl func acc dict


newMapTable : (Int -> a -> b) -> Table a -> Table b
newMapTable func (Table dict) =
    Table (Dict.map func dict)


emptyComponentTable : Table a
emptyComponentTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent (EntityId entityId) component (Table dict) =
    Table (Dict.insert entityId component dict)


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict


mapComponents : (Int -> a -> b) -> Table a -> Table b
mapComponents f (Table dict) =
    Table (Dict.map f dict)


mapComponent : (a -> res) -> Table a -> EntityId -> Maybe res
mapComponent f tableA entityId =
    Maybe.map
        f
        (getComponent tableA entityId)


map2Component : (a -> b -> c) -> ( Table a, Table b ) -> EntityId -> Maybe c
map2Component f ( tableA, tableB ) entityId =
    Maybe.map2
        f
        (getComponent tableA entityId)
        (getComponent tableB entityId)


map3Component : (a -> b -> c -> d) -> ( Table a, Table b, Table c ) -> EntityId -> Maybe d
map3Component f ( tableA, tableB, tableC ) entityId =
    Maybe.map3
        f
        (getComponent tableA entityId)
        (getComponent tableB entityId)
        (getComponent tableC entityId)


mapTable : (a -> a) -> EntityId -> Table a -> Table a
mapTable f entityId tableA =
    case mapComponent f tableA entityId of
        Just a ->
            setComponent entityId a tableA

        Nothing ->
            tableA


mapTable2 : (a -> b -> ( a, b )) -> EntityId -> ( Table a, Table b ) -> ( Table a, Table b )
mapTable2 f entityId ( tableA, tableB ) =
    case map2Component f ( tableA, tableB ) entityId of
        Just ( a, b ) ->
            ( setComponent entityId a tableA, setComponent entityId b tableB )

        Nothing ->
            ( tableA, tableB )


update2ComponentsTablesFromEntityList : ( Table a, Table b ) -> List ( EntityId, a, b ) -> ( Table a, Table b )
update2ComponentsTablesFromEntityList ( tableA, tableB ) entityList =
    ( List.foldl
        (\( entityId, a, _ ) table -> setComponent entityId a table)
        tableA
        entityList
    , List.foldl
        (\( entityId, _, b ) table -> setComponent entityId b table)
        tableB
        entityList
    )


mapToEntityList : EntityTable -> Table a -> List ( EntityId, a )
mapToEntityList entityTable componentTable =
    filterMapEntityTable
        (\id -> getIdComponent componentTable id)
        entityTable


getIdComponent : Table a -> EntityId -> Maybe ( EntityId, a )
getIdComponent componentTable entityId =
    mapComponent (\a -> ( entityId, a )) componentTable entityId


map2ToEntityList : EntityTable -> ( Table a, Table b ) -> List ( EntityId, a, b )
map2ToEntityList entityTable componentTables =
    filterMapEntityTable
        (\id -> get2Components componentTables id)
        entityTable



--map3ToEntityList : EntityTable -> ( Table a, Table b, Table c ) -> List ( EntityId, a, b, c )
--map3ToEntityList entityTable componentTables =
--    filterMapEntityTable
--        (\id -> get3Components componentTables id)
--        entityTable


get2Components : ( Table a, Table b ) -> EntityId -> Maybe ( EntityId, a, b )
get2Components ( positionTable, velocityTable ) entityId =
    map2Component (\a b -> ( entityId, a, b )) ( positionTable, velocityTable ) entityId



--get3Components : ( Table a, Table b, Table c ) -> EntityId -> Maybe ( EntityId, a, b, c )
--get3Components ( tableA, tableB, tableC ) entityId =
--    map3Component (\a b c -> ( entityId, a, b, c )) ( tableA, tableB, tableC ) entityId
