module SystemAcceleration exposing (updateWorld)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)
import World exposing (World)


type alias Components =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


updateWorld : World -> World
updateWorld world =
    updateEntities
        { updateComponents = updatePlayerVelocity
        , updateWorld =
            \entityId components accWorld ->
                { accWorld
                    | keyboardInputComponents = setComponent entityId components.keyboardInput accWorld.keyboardInputComponents
                    , velocityComponents = setComponent entityId components.velocity accWorld.velocityComponents
                }
        , world = world
        , entityTable = world.entities
        , componentTables =
            Components
                |> from world.keyboardInputComponents
                |> join world.velocityComponents
        }


updatePlayerVelocity : EntityId -> Components -> Components
updatePlayerVelocity _ components =
    { keyboardInput = components.keyboardInput
    , velocity =
        case components.keyboardInput.key of
            Just key ->
                updateVelocityFromKey key components.velocity

            Nothing ->
                components.velocity
    }


updateVelocityFromKey : Key -> ComponentVelocity -> ComponentVelocity
updateVelocityFromKey key velocity =
    case key of
        KeyboardInput.ArrowLeft ->
            { x = velocity.x - 1, y = velocity.y }

        KeyboardInput.ArrowRight ->
            { x = velocity.x + 1, y = velocity.y }

        KeyboardInput.ArrowUp ->
            { x = velocity.x, y = velocity.y + 1 }

        KeyboardInput.ArrowDown ->
            { x = velocity.x, y = velocity.y - 1 }

        KeyboardInput.Other ->
            velocity
