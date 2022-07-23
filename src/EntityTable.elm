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
    { a : Table a
    , b : Table b
    }


bothStep : Int -> a -> b -> Table (Component2 a b) -> Table (Component2 a b)
bothStep id a b (Table dict) =
    Table (Dict.insert id { a = a, b = b } dict)


getDictTable : Table a -> Dict Int a
getDictTable (Table dict) =
    dict


unionTable : Table a -> Table a -> Table a
unionTable (Table dictHighPriority) (Table dictLowPriority) =
    Table (Dict.union dictHighPriority dictLowPriority)


splitTable2 : Table (Component2 a b) -> Table2 a b
splitTable2 table =
    foldlTable
        (\id comp2 table2 -> { a = insertInTable id comp2.a table2.a, b = insertInTable id comp2.b table2.b })
        { a = emptyComponentTable, b = emptyComponentTable }
        table


intersectTable2 : Table2 a b -> Table (Component2 a b)
intersectTable2 { a, b } =
    Dict.merge (\_ _ t -> t) bothStep (\_ _ t -> t) (getDictTable a) (getDictTable b) emptyComponentTable


unionTable2 : Table2 a b -> Table2 a b -> Table2 a b
unionTable2 tableHighPriority tableLowPriority =
    { a = unionTable tableHighPriority.a tableLowPriority.a
    , b = unionTable tableHighPriority.b tableLowPriority.b
    }


update2Tables : (Table (Component2 a b) -> Table (Component2 a b)) -> Table2 a b -> Table2 a b
update2Tables func table2 =
    let
        updated =
            splitTable2 (createFromTable2 func table2)
    in
    unionTable2 updated table2


createFromTable2 : (Table (Component2 a b) -> Table c) -> Table2 a b -> Table c
createFromTable2 func table2 =
    func (intersectTable2 table2)


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


insertInTable : Int -> a -> Table a -> Table a
insertInTable id a (Table dict) =
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
setComponent (EntityId entityId) component table =
    insertInTable entityId component table


getComponent : Table a -> EntityId -> Maybe a
getComponent (Table dict) (EntityId id) =
    Dict.get id dict


mapComponents : (Int -> a -> b) -> Table a -> Table b
mapComponents f (Table dict) =
    Table (Dict.map f dict)


