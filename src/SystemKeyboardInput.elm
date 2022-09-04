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
                entityId
                { key = Just key }
                world.keyboardInputComponents
    }


clear : EntityId -> World -> World
clear entityId world =
    { world
        | keyboardInputComponents =
            updateComponent
                entityId
                { key = Nothing }
                world.keyboardInputComponents
    }
