module SystemAcceleration exposing (systemAcceleration)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (EntityTable, Table, foldlEntityTable, mapTable2)
import KeyboardInput exposing (Key)


systemAcceleration : EntityTable -> ( Table ComponentKeyboardInput, Table ComponentVelocity ) -> ( Table ComponentKeyboardInput, Table ComponentVelocity )
systemAcceleration entityTable componentTables =
    foldlEntityTable
        (mapTable2 updatePlayerVelocity)
        componentTables
        entityTable


updatePlayerVelocity : ComponentKeyboardInput -> ComponentVelocity -> ( ComponentKeyboardInput, ComponentVelocity )
updatePlayerVelocity keyboardInput position =
    case keyboardInput.key of
        Just key ->
            ( keyboardInput, updateVelocityFromKey key position )

        Nothing ->
            ( keyboardInput, position )


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
