module SystemAcceleration exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Context as Context
import Core.Modifier as Modifier
import KeyboardInput exposing (Key)


type alias OutputComponents =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


type alias InputComponents =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


clearVelocity entityId context =
    ComponentVelocity.modifier.map (ComponentTable.insert entityId ComponentVelocity.identity) context


updateEntity =
    Context.updateComponents
        { func = updatePlayerVelocity
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentKeyboardInput.modifier.get
                |> Component.join ComponentVelocity.modifier.get
        , output =
            Modifier.select
                |> ComponentTable.joinModifier ( ComponentKeyboardInput.modifier.map, .keyboardInput )
                |> ComponentTable.joinModifier ( ComponentVelocity.modifier.map, .velocity )
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
