module SystemAcceleration exposing (updateWorld)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import CustomTuple exposing (..)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)
import World exposing (World)



updateWorld : World -> World
updateWorld world =
    let
        tables =
            updateEachEntity2
                updatePlayerVelocity
                world.entities
                world.keyboardInputComponents
                world.velocityComponents
    in
    { world
        | keyboardInputComponents = tables.first
        , velocityComponents = tables.second
    }


updatePlayerVelocity : EntityId -> ComponentKeyboardInput -> ComponentVelocity -> Tuple2 ComponentKeyboardInput ComponentVelocity
updatePlayerVelocity _ keyboardInput velocity =
    case keyboardInput.key of
        Just key ->
            toTuple2 keyboardInput (updateVelocityFromKey key velocity)

        Nothing ->
            toTuple2 keyboardInput velocity


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
