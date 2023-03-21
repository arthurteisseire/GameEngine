module Core.Modifier exposing (..)

import Core.ComponentTable as ComponentTable exposing (ComponentTable)
import Core.EntityId exposing (EntityId)


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
    ( (ComponentTable a -> ComponentTable a) -> db -> db, b -> a )
    -> (b -> EntityId -> db -> db)
    -> b
    -> EntityId
    -> db
    -> db
join mapTable previousUpdater outputComponents entityId world =
    world
        |> previousUpdater outputComponents entityId
        |> updateComponentInTable mapTable outputComponents entityId


updateComponentInTable : ( (ComponentTable a -> ComponentTable a) -> db -> db, b -> a ) -> b -> EntityId -> db -> db
updateComponentInTable ( mapTable, getA ) outputComponents entityId =
    mapTable (ComponentTable.insert entityId (getA outputComponents))
