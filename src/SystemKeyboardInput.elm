module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import Core.EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)
import KeyboardInput exposing (Key)
import World exposing (World)


read : Key -> EntityId -> World -> World
read key entityId world =
    { world
        | keyboardInputComponents =
            Table.insert entityId { key = Just key } world.keyboardInputComponents
    }


clear : EntityId -> World -> World
clear entityId world =
    { world
        | keyboardInputComponents =
            Table.insert entityId { key = Nothing } world.keyboardInputComponents
    }
