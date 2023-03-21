module Core.ComponentTable exposing (..)

import Core.EntityId as EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)


type ComponentTable a
    = ComponentTable (Table a) (ComponentOps a)


type alias ComponentOps a =
    { toString : a -> String
    }


select : a -> a
select =
    Table.select


from : Table a -> (a -> result) -> Table result
from table func =
    Table.from table func


innerJoin : ComponentTable a -> Table (a -> result) -> Table result
innerJoin componentTable nextTable =
    Table.innerJoin (getTable componentTable) nextTable


toString : ComponentTable a -> String
toString (ComponentTable table ops) =
    "Table(\n"
        ++ Table.foldl
            (\entityId component accStr ->
                accStr
                    ++ "(EntityId="
                    ++ EntityId.toString entityId
                    ++ ", "
                    ++ ops.toString component
                    ++ "\n"
            )
            ""
            table
        ++ ")"


empty : ComponentOps a -> ComponentTable a
empty ops =
    ComponentTable Table.empty ops


get : EntityId -> ComponentTable a -> Maybe a
get entityId (ComponentTable table _) =
    Table.get entityId table


insert : EntityId -> a -> ComponentTable a -> ComponentTable a
insert entityId a (ComponentTable table ops) =
    ComponentTable (Table.insert entityId a table) ops


remove : EntityId -> ComponentTable a -> ComponentTable a
remove entityId (ComponentTable table ops) =
    ComponentTable (Table.remove entityId table) ops


mapRow : EntityId -> (a -> result) -> ComponentTable a -> Maybe result
mapRow entityId func table =
    Maybe.map func (get entityId table)


values : ComponentTable a -> List a
values (ComponentTable table _) =
    Table.values table


getTable : ComponentTable a -> Table a
getTable (ComponentTable table _) =
    table


getOps : ComponentTable a -> ComponentOps a
getOps (ComponentTable _ ops) =
    ops
