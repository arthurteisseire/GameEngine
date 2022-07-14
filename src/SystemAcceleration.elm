module SystemAcceleration exposing (update)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)


update : Table2 ComponentKeyboardInput ComponentVelocity -> Table2 ComponentKeyboardInput ComponentVelocity
update table2 =
    update2Tables
        (newMapTable (\_ component2 -> updatePlayerVelocity component2))
        table2


updatePlayerVelocity : Component2 ComponentKeyboardInput ComponentVelocity -> Component2 ComponentKeyboardInput ComponentVelocity
updatePlayerVelocity component2 =
    let
        keyboardInput =
            component2.a

        velocity =
            component2.b
    in
    case keyboardInput.key of
        Just key ->
            { a = keyboardInput, b = updateVelocityFromKey key velocity }

        Nothing ->
            { a = keyboardInput, b = velocity }


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
