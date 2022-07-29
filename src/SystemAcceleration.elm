module SystemAcceleration exposing (update)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)


update :
    EntityTable
    -> Table ComponentKeyboardInput
    -> Table ComponentVelocity
    -> Table2 ComponentKeyboardInput ComponentVelocity
update entityTable keyboardInputTable velocityTable =
    updateEachEntity2
        updatePlayerVelocity
        entityTable
        keyboardInputTable
        velocityTable


updatePlayerVelocity :
    EntityId
    -> ComponentKeyboardInput
    -> ComponentVelocity
    -> Component2 ComponentKeyboardInput ComponentVelocity
updatePlayerVelocity _ keyboardInput velocity =
    case keyboardInput.key of
        Just key ->
            toComponent2 keyboardInput (updateVelocityFromKey key velocity)

        Nothing ->
            toComponent2 keyboardInput velocity


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
