module SystemPlayerVelocity exposing (..)

import ComponentPlayer exposing (ComponentPlayer)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (EntityTable, Table, foldlEntityTable, mapTable2)
import KeyboardInput exposing (Key)


systemUpdatePlayerVelocity : Maybe Key -> Float -> EntityTable -> ( Table ComponentPlayer, Table ComponentVelocity ) -> ( Table ComponentPlayer, Table ComponentVelocity )
systemUpdatePlayerVelocity maybeKey dt entityTable componentTables =
    foldlEntityTable
        (mapTable2 (updatePlayerVelocity maybeKey dt))
        componentTables
        entityTable


updatePlayerVelocity : Maybe Key -> Float -> ComponentPlayer -> ComponentVelocity -> ( ComponentPlayer, ComponentVelocity )
updatePlayerVelocity maybeKey dt player position =
    case maybeKey of
        Just key ->
            ( player, updateVelocityFromKey key position )

        Nothing ->
            ( player, position )


updateVelocityFromKey : Key -> ComponentVelocity -> ComponentVelocity
updateVelocityFromKey key velocity =
    case key of
        KeyboardInput.Left ->
            { x = velocity.x - 1, y = velocity.y }

        KeyboardInput.Right ->
            { x = velocity.x + 1, y = velocity.y }

        KeyboardInput.Up ->
            { x = velocity.x, y = velocity.y + 1 }

        KeyboardInput.Down ->
            { x = velocity.x, y = velocity.y - 1 }

        KeyboardInput.Other ->
            velocity
