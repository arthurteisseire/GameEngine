module SystemDie exposing (..)

import ComponentLife exposing (ComponentLife)
import EntityTable exposing (..)
import World exposing (World)


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    { world
        | entities =
            case getComponent entityId world.lifeComponents of
                Just life ->
                    removeIf (life.healPoints <= 0) entityId world.entities

                Nothing ->
                    world.entities
    }


updateWorld : World -> World
updateWorld world =
    let
        updatedEntities : EntitySet
        updatedEntities =
            filterEntities
                (\entityId ->
                    case getComponent entityId world.lifeComponents of
                        Just life ->
                            life.healPoints > 0

                        Nothing ->
                            True
                )
                world.entities
    in
    { world
        | entities = updatedEntities
    }
