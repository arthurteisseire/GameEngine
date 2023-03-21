module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import Core.ComponentTable as ComponentTable
import Core.EntityId exposing (EntityId)
import KeyboardInput exposing (Key)
import World exposing (World)


read : Key -> EntityId -> World -> World
read key entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Just key })


clear : EntityId -> World -> World
clear entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Nothing })
