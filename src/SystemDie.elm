module SystemDie exposing (..)

import Core.EntityId exposing (EntityId)
import Core.EntitySet as EntitySet exposing (EntitySet)
import Core.Table as Table exposing (Table)
import World exposing (World)


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    case Table.get entityId world.lifeComponents of
        Just life ->
            if life.healPoints <= 0 then
                removeEntity entityId world

            else
                world

        Nothing ->
            world


removeEntity : EntityId -> World -> World
removeEntity entityId world =
    let
        newWorld =
            World.remove entityId world
    in
    { newWorld
        | entityIdDebug =
            case world.entityIdDebug of
                Just id ->
                    if id == entityId then
                        Nothing

                    else
                        world.entityIdDebug

                Nothing ->
                    Nothing
    }
