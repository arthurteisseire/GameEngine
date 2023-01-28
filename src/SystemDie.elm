module SystemDie exposing (..)

import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet)
import Core.Table as Table exposing (Table)
import World exposing (World)


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    { world
        | entities =
            case Table.get entityId world.lifeComponents of
                Just life ->
                    EntitySet.removeIf (life.healPoints <= 0) entityId world.entities

                Nothing ->
                    world.entities
    }
