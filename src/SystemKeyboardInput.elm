module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import Core.EntityId exposing (EntityId)
import Core.Table as Table exposing (Table)
import KeyboardInput exposing (Key)
import World exposing (World)


read : Key -> EntityId -> World -> World
read key entityId =
    World.keyboardInputModifier.map
        (Table.insert entityId { key = Just key })


clear : EntityId -> World -> World
clear entityId =
    World.keyboardInputModifier.map
        (Table.insert entityId { key = Nothing })
