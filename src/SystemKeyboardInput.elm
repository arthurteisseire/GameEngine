module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import Core.ComponentTable as ComponentTable
import Core.EntityId exposing (EntityId)
import KeyboardInput exposing (Key)


read key entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Just key })


clear entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Nothing })
