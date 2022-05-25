module SystemMovePlayer exposing (..)

import ComponentPlayer exposing (ComponentPlayer)
import ComponentPosition exposing (ComponentPosition)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)


movePlayerSystem : Maybe Key -> Float -> EntityTable -> ( Table ComponentPlayer, Table ComponentPosition ) -> ( Table ComponentPlayer, Table ComponentPosition )
movePlayerSystem maybeKey dt entityTable componentTables =
    foldlEntityTable
        (mapTable2 (updatePlayerPosition maybeKey dt))
        componentTables
        entityTable


updatePlayerPosition : Maybe Key -> Float -> ComponentPlayer -> ComponentPosition -> ( ComponentPlayer, ComponentPosition )
updatePlayerPosition maybeKey dt player position =
    case maybeKey of
        Just key ->
            ( player, movePlayerFromKey key position )

        Nothing ->
            ( player, position )


movePlayerFromKey : Key -> ComponentPosition -> ComponentPosition
movePlayerFromKey key position =
    case key of
        KeyboardInput.Left ->
            ComponentPosition.mapX (\x -> x - 1) position

        KeyboardInput.Right ->
            ComponentPosition.mapX (\x -> x + 1) position

        KeyboardInput.Up ->
            ComponentPosition.mapY (\y -> y + 1) position

        KeyboardInput.Down ->
            ComponentPosition.mapY (\y -> y - 1) position

        KeyboardInput.Other ->
            position
