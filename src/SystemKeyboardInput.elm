module SystemKeyboardInput exposing (..)

import ComponentKeyboardInput exposing (ComponentKeyboardInput)
import Core.ComponentTable as ComponentTable


read key entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Just key })


clear entityId =
    ComponentKeyboardInput.modifier.map
        (ComponentTable.insert entityId { key = Nothing })
