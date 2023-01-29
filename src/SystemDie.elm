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
    { entities = EntitySet.remove entityId world.entities
    , keyboardInputComponents = Table.remove entityId world.keyboardInputComponents
    , positionComponents = Table.remove entityId world.positionComponents
    , velocityComponents = Table.remove entityId world.velocityComponents
    , lifeComponents = Table.remove entityId world.lifeComponents
    , visualComponents = Table.remove entityId world.visualComponents
    , attackComponents = Table.remove entityId world.attackComponents
    , damageComponents = Table.remove entityId world.damageComponents
    , animationComponents = Table.remove entityId world.animationComponents
    , turnComponents = Table.remove entityId world.turnComponents
    , terrainComponents = Table.remove entityId world.terrainComponents
    , aiComponents = Table.remove entityId world.aiComponents
    , playerComponents = Table.remove entityId world.playerComponents
    , entityIdDebug =
        case world.entityIdDebug of
            Just id ->
                if id == entityId then
                    Nothing

                else
                    world.entityIdDebug

            Nothing ->
                Nothing
    , isPause = world.isPause
    }
