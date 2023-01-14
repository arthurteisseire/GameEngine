module SystemAcceleration exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)
import World exposing (..)


type alias OutputComponents =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


type alias InputComponents =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


clearVelocity : EntityId -> World -> World
clearVelocity entityId world =
    { world | velocityComponents = insertComponent entityId ComponentVelocity.identity world.velocityComponents }


updateEntity : EntityId -> World -> World
updateEntity =
    updateComponents
        { func = updatePlayerVelocity
        , inputComponents =
            toInputComponents InputComponents
                |> withInput .keyboardInputComponents
                |> withInput .velocityComponents
        , output =
            toOutputComponents
                |> withOutput keyboardInputComponent
                |> withOutput velocityComponent
        }


updatePlayerVelocity : InputComponents -> OutputComponents
updatePlayerVelocity components =
    { keyboardInput = ComponentKeyboardInput.identity
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

        _ ->
            velocity
