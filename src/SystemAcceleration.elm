module SystemAcceleration exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import ComponentVelocity exposing (ComponentVelocity)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)
import World exposing (World)


type alias Components =
    { keyboardInput : ComponentKeyboardInput
    , velocity : ComponentVelocity
    }


clearVelocity : EntityId -> World -> World
clearVelocity entityId world =
    { world | velocityComponents = insertComponent entityId ComponentVelocity.identity world.velocityComponents }


updateEntity : EntityId -> World -> World
updateEntity entityId world =
    Maybe.withDefault world <|
        Maybe.map2
            (\keyboardInput velocity ->
                let
                    components =
                        updatePlayerVelocity entityId (Components keyboardInput velocity)
                in
                { world
                    | keyboardInputComponents = insertComponent entityId components.keyboardInput world.keyboardInputComponents
                    , velocityComponents = insertComponent entityId components.velocity world.velocityComponents
                }
            )
            (getComponent entityId world.keyboardInputComponents)
            (getComponent entityId world.velocityComponents)


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
