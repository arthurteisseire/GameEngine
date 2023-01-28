module Core.Component exposing (..)

import Core.EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)


select : a -> EntityId -> db -> Maybe a
select a _ _ =
    Just a


join : (db -> Table a) -> (EntityId -> db -> Maybe (a -> b)) -> EntityId -> db -> Maybe b
join getTable nestedFunc entityId db =
    nestedFunc entityId db
        |> Maybe.andThen
            (\func -> Table.mapRow entityId func (getTable db))
