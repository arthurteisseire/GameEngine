module SytemMove exposing (..)

import ComponentPosition exposing (ComponentPosition)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)


systemMove : Float -> EntityTable -> ( Table ComponentVelocity, Table ComponentPosition ) -> ( Table ComponentVelocity, Table ComponentPosition )
systemMove dt entityTable componentTables =
    foldlEntityTable
        (mapTable2 (move dt))
        componentTables
        entityTable


move : Float -> ComponentVelocity -> ComponentPosition -> ( ComponentVelocity, ComponentPosition )
move dt velocity position =
    ( ComponentVelocity.identity, { x = position.x + velocity.x, y = position.y + velocity.y } )

