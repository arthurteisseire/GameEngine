module SystemDie exposing (..)

import ComponentLife exposing (ComponentLife)
import EntityTable exposing (..)
import World exposing (World)


updateWorld : World -> World
updateWorld world =
    let
        updatedEntities : EntityTable
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
