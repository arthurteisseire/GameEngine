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


mapEntityTable : (EntityId -> a) -> EntityTable -> List a
mapEntityTable f (EntityTable _ entities) =
    List.map f entities



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
    { a : Table a
    , b : Table b
    }


bothStep : Int -> a -> b -> Table (Component2 a b) -> Table (Component2 a b)
bothStep id a b (Table dict) =
    Table (Dict.insert id { a = a, b = b } dict)


getDictTable : Table a -> Dict Int a
getDictTable (Table dict) =
    dict


table2ToMatchingEntities : Table2 a b -> Table (Component2 a b)
table2ToMatchingEntities { a, b } =
    Dict.merge (\_ _ t -> t) bothStep (\_ _ t -> t) (getDictTable a) (getDictTable b) emptyComponentTable


unionTables : Table2 a b -> Table2 a b -> Table2 a b
unionTables tableHighPriority tableLowPriority =
    { a = unionTable tableHighPriority.a tableLowPriority.a
    , b = unionTable tableHighPriority.b tableLowPriority.b
    }


unionTable : Table a -> Table a -> Table a
unionTable (Table dictHighPriority) (Table dictLowPriority) =
    Table (Dict.union dictHighPriority dictLowPriority)


splitTable : Table (Component2 a b) -> Table2 a b
splitTable table =
    foldlTable
        (\id comp2 table2 -> { a = insertInTable id comp2.a table2.a, b = insertInTable id comp2.b table2.b })
        { a = emptyComponentTable, b = emptyComponentTable }
        table


update2Tables : EntityTable -> (Table (Component2 a b) -> Table (Component2 a b)) -> Table2 a b -> Table2 a b
update2Tables entities func table2 =
    let
        updated =
            splitTable (func (filterEntities entities (table2ToMatchingEntities table2)))
    in
    unionTables updated table2


filterEntities : EntityTable -> Table a -> Table a
filterEntities entities table =
    filterTable (\id _ -> doesEntityExist (EntityId id) entities) table


filterTable : (Int -> a -> Bool) -> Table a -> Table a
filterTable isGood (Table dict) =
    Table (Dict.filter isGood dict)


insertInTable : Int -> a -> Table a -> Table a
insertInTable id a (Table dict) =
    Table (Dict.insert id a dict)


foldlTable : (Int -> a -> b -> b) -> b -> Table a -> b
foldlTable func acc (Table dict) =
    Dict.foldl func acc dict


valuesTable : Table a -> List a
valuesTable (Table dict) =
    Dict.values dict


newMapTable : (Int -> a -> b) -> Table a -> Table b
newMapTable func (Table dict) =
    Table (Dict.map func dict)


emptyComponentTable : Table a
emptyComponentTable =
    Table Dict.empty


setComponent : EntityId -> a -> Table a -> Table a
setComponent (EntityId entityId) component table =
    insertInTable entityId component table


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict


mapComponents : (Int -> a -> b) -> Table a -> Table b
mapComponents f (Table dict) =
    Table (Dict.map f dict)


map2Component : (a -> b -> c) -> ( Table a, Table b ) -> EntityId -> Maybe c
map2Component f ( tableA, tableB ) entityId =
    Maybe.map2
        f
        (getComponent tableA entityId)
        (getComponent tableB entityId)
