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
                    removeEntityIf (life.healPoints <= 0) entityId world.entities

                Nothing ->
                    world.entities
    }
