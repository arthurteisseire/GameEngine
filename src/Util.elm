module Util exposing (..)

import Core.EntitySet as EntitySet


applySystem updateEntity entitySet world =
    EntitySet.foldl
        updateEntity
        world
        entitySet


applySystemWithOperations updateEntity entitySet contextOperations world =
    EntitySet.foldl
        (updateEntity contextOperations)
        world
        entitySet
