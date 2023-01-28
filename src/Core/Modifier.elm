module Core.Modifier exposing (..)

import Core.EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)


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
tableModifier modifier =
    { get = modifier.get
    , set = modifier.set
    , map = \func b -> modifier.set (func (modifier.get b)) b
    }


select : b -> EntityId -> db -> db
select _ _ db =
    db


join :
    ( Modifier (Table a) db, b -> a )
    -> (b -> EntityId -> db -> db)
    -> b
    -> EntityId
    -> db
    -> db
join modifier previousUpdater outputComponents entityId world =
    world
        |> previousUpdater outputComponents entityId
        |> updateComponentInTable modifier outputComponents entityId


updateComponentInTable : ( Modifier (Table a) db, b -> a ) -> b -> EntityId -> db -> db
updateComponentInTable ( tableA, getA ) outputComponents entityId =
    tableA.map (Table.insert entityId (getA outputComponents))
