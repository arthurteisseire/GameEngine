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
