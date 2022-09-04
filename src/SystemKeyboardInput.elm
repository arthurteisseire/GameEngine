module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import EntityTable exposing (..)
import KeyboardInput exposing (Key)
import World exposing (World)


read : Key -> EntityId -> World -> World
read key entityId world =
    { world
        | keyboardInputComponents =
            updateComponent
                (\_ -> { key = Just key })
                entityId
                world.keyboardInputComponents
    }


clear : EntityId -> World -> World
clear entityId world =
    { world
        | keyboardInputComponents =
            updateComponent
                (\_ -> { key = Nothing })
                entityId
                world.keyboardInputComponents
    }
