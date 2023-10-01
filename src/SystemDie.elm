module SystemDie exposing (..)

import Core.ComponentTable as ComponentTable
import Core.EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)


updateEntity contextOperations entityId context =
    case Table.get entityId (ComponentTable.getTable context.lifeComponents) of
        Just life ->
            if life.healPoints <= 0 then
                removeEntity contextOperations entityId context

            else
                context

        Nothing ->
            context


removeEntity contextOperations entityId context =
    let
        newWorld =
            contextOperations.remove entityId context
    in
    { newWorld
        | entityIdDebug =
            case context.entityIdDebug of
                Just id ->
                    if id == entityId then
                        Nothing

                    else
                        context.entityIdDebug

                Nothing ->
                    Nothing
    }
