module SystemAcceleration exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import Core.Component as Component
import Core.ComponentTable as ComponentTable
import Core.Database as Db
import Core.EntityId exposing (EntityId)
import Core.Modifier as Modifier
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
    ComponentVelocity.modifier.map (ComponentTable.insert entityId ComponentVelocity.identity) world


updateEntity : EntityId -> World -> World
updateEntity =
    Db.updateComponents
        { func = updatePlayerVelocity
        , inputComponents =
            Component.select InputComponents
                |> Component.join ComponentKeyboardInput.modifier.get
                |> Component.join ComponentVelocity.modifier.get
        , output =
            Modifier.select
                |> Modifier.join ( ComponentKeyboardInput.modifier.map, .keyboardInput )
                |> Modifier.join ( ComponentVelocity.modifier.map, .velocity )
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
