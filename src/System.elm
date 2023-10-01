module System exposing (..)

import Core.EntitySet as EntitySet


run updateEntity entitySet world =
    EntitySet.foldl
        updateEntity
        world
        entitySet


runWithOps updateEntity entitySet contextOperations world =
    EntitySet.foldl
        (updateEntity contextOperations)
        world
        entitySet
