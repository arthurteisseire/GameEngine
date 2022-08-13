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
            \entityId { keyboardInput, velocity } accWorld ->
                { accWorld
                    | keyboardInputComponents = setComponent entityId keyboardInput accWorld.keyboardInputComponents
                    , velocityComponents = setComponent entityId velocity accWorld.velocityComponents
                }
        , world = world
        , entityTable = world.entities
        , componentTables = intersectTable2 Components world.entities world.keyboardInputComponents world.velocityComponents
        }


updatePlayerVelocity : EntityId -> Components -> Components
updatePlayerVelocity _ { keyboardInput, velocity } =
    let
        updatedVelocity =
            case keyboardInput.key of
                Just key ->
                    updateVelocityFromKey key velocity

                Nothing ->
                    velocity
    in
    { keyboardInput = keyboardInput
    , velocity = updatedVelocity
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
